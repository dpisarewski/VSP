%% Copyright
-module(client).
-author("dpisarewski windowsfreak").

%% API
-export([start/2,startMultiple/3]).

%Startfunktion zum Starten mehrerer Clients
startMultiple(ServerPID, ClientNummer, Remaining) when Remaining > 0 ->
	Fun = spawn(fun() -> start(ServerPID, ClientNummer) end),
	startMultiple(ServerPID, ClientNummer + 1, Remaining - 1);
startMultiple(ServerPID, _, _) -> ServerPID.
	
%Startfunktion. Wird als allererstes gestartet
%Es werden die Konfigurationsdatei eingelesen
%und Parameter von dem Client-Prozess gesetzt
start(ServerPID, ClientNummer) ->
  %Datei in der die Einstellungen Hinterlegt
  %%  sind
  {ok, ConfigListe} = file:consult("client.cfg"),
  %Der Pfad und Name der Datei für Logging
  LogDatei = lists:concat(["client_", ClientNummer, "@", element(2, inet:gethostname()), ".log"]),
  %Intervall mit dem die Nachrichten versendet werden
  {ok, SendeIntervall} = werkzeug:get_config_value(intervall, ConfigListe),
  %Anzahl der Nachrichten die hintereinander versendet werden
  {ok, AnzahlNachrichten} = werkzeug:get_config_value(anzahl_nachrichten, ConfigListe),
  %Zeit die der Client "lebt"
  {ok, LifeTime} = werkzeug:get_config_value(life_time, ConfigListe),

  %Prozess der gesendete und empfangene Nachrichten speichert
  NachrichtenSammler = spawn(fun() -> nachrichtenNummern([]) end),
  %Starten des Clients und anschliessendes senden des Stopsignals um den Timer zu starten
  spawn(fun() ->
    timer:exit_after(LifeTime * 1000, "Client beendet"),
    simulation(AnzahlNachrichten, ServerPID, LogDatei, SendeIntervall, ClientNummer, NachrichtenSammler)
  end)
.

%Schleife die der Client immerwieder durchläuft
%%  bis der Timer abläeft
simulation(AnzahlNachrichten, ServerPID, LogDatei, SendeIntervall, ClientNummer, NachrichtenSammler) ->

  %Anstossen der Sendefunktion
  werkzeug:logging(LogDatei, "\n%%%%%%%%%%%%%%%%%%%%%\n% client: sendet Nachrichten\n%%%%%%%%%%%%%%%%%%%%%"),
  sendeNachricht(ServerPID, LogDatei, AnzahlNachrichten, SendeIntervall, ClientNummer, NachrichtenSammler),

  %Fehlernachricht provozieren
  ServerPID ! {getmsgid, self()},
  receive {nnr, Number1} -> Number1 end,
  werkzeug:logging(LogDatei, integer_to_list(ClientNummer) ++ "-client : " ++ integer_to_list(Number1) ++ "te_Nachricht um " ++ werkzeug:timeMilliSecond() ++ " vergessen zu senden!!!\n"),

  %Nachrichten abfragen
  werkzeug:logging(LogDatei, "%%%%%%%%%%%%%%%%%%%%%\n% client: fragt Nachrichten ab\n%%%%%%%%%%%%%%%%%%%%%\n\n"),
  frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler),

  %Neues Intervall für das Versenden der Nachrichten berechnen
  NeuerSendeIntervall = berechneIntervall(SendeIntervall),

  simulation(AnzahlNachrichten, ServerPID, LogDatei, NeuerSendeIntervall, ClientNummer, NachrichtenSammler)
.

%Funktion zum Abfragen der Nachrichten
frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler) ->
  ServerPID ! {getmessages, self()},
  receive
    {reply, Number, Nachricht, Terminated} ->

      %Prüfung ob die Nachricht die eigene ist
      NachrichtenSammler ! {istEigeneNachricht, Number, self()},

      %falls eigene Nachricht mit ****** Markieren
      receive
        ok -> NeueNachricht = Nachricht ++ "C In: " ++ werkzeug:timeMilliSecond() ++ "******";
        nok -> NeueNachricht = Nachricht ++ "C In: " ++ werkzeug:timeMilliSecond()
      end,

      %empfangene Nachricht Protokollieren
      werkzeug:logging(LogDatei, NeueNachricht ++ "\n"),

      %Prüfung ob es weitere unbekannte Nachrichten gibt
      case Terminated of
        true -> true;
        false -> frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler)
      end
  end
.

%Funktion zum senden von Nachrichten. Sendet Nachrichten
%%  solange bis alle gesendet sind
sendeNachricht(ServerPID, LogDatei, AnzahlNachrichten, SendeIntervall, ClientNummer, NachrichtenSammler) when AnzahlNachrichten > 0 ->
  ServerPID ! {getmsgid, self()},

  receive
    {nnr, Number} -> Number
  end,

  %Zu sendende Nachricht zusammenstellen
  {ok, Hostname} = inet:gethostname(),
  Nachricht = lists:concat(["\n", ClientNummer, "-client@", Hostname, "2", "06", " : ", Number, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()]),

  ServerPID ! {dropmessage, {Nachricht, Number}},
  werkzeug:logging(LogDatei, Nachricht ++ "\n"),

  NachrichtenSammler ! {add, Number},

  %Wartezeit zwischen den Nachrichten berechnen und solange warten
  timer:sleep(round(SendeIntervall*1000)),

  sendeNachricht(ServerPID, LogDatei, AnzahlNachrichten-1, SendeIntervall, ClientNummer, NachrichtenSammler)
;
sendeNachricht(ServerPID, _, _, _, _, _) -> ServerPID.

%Funktion zur Berechnung des Intervalls zwischen
%%  dem Versenden der Nachrichten
berechneIntervall(Intervall) ->
  %Zufallszahl erzeugen. -50% / +50%
  Sign = round(random:uniform()) * 2 - 1,
  Change = max(1.0, Intervall * 0.5),
  max(1.0, Intervall + Change * Sign)
.

%Speichert die Nummern der gesendeten
%%  Nachrichten und vergleicht diese auf Anfrage
nachrichtenNummern(Liste) ->
  receive
    {add, Nummer} ->
      NeueListe = [Nummer | Liste],
      nachrichtenNummern(NeueListe);

    {istEigeneNachricht, Nummer, Pid} ->
      Flag = lists:member(Nummer, Gesendete),
      if
        Flag == true -> Pid ! ok;
        Flag == false ->  Pid ! nok
      end,
      nachrichtenNummern(Liste)
  end
.