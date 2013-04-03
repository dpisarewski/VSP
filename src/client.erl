-module(client).
-author("dpisarewski windowsfreak").
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
  %Anzahl der Nachrichten die hintereinander versendet werden
  {ok, AnzahlNachrichten} = werkzeug:get_config_value(anzahl_nachrichten, ConfigListe),
  %Intervall mit dem die Nachrichten versendet werden
  {ok, SendeIntervall} = werkzeug:get_config_value(intervall, ConfigListe),
  %Zeit die der Client "lebt"
  {ok, LifeTime} = werkzeug:get_config_value(life_time, ConfigListe),

  %Prozess der gesendete und empfangene Nachrichten speichert
  NummernListe = spawn(fun() -> liste([]) end),
  %Starten des Clients und anschliessendes senden des Stopsignals um den Timer zu starten
  spawn(fun() ->
    timer:exit_after(LifeTime * 1000, "Client beendet"),
    simulation(ServerPID, LogDatei, AnzahlNachrichten, SendeIntervall, ClientNummer, NummernListe)
  end)
.

%Schleife die der Client immerwieder durchläuft
%%  bis der Timer abläeft
simulation(ServerPID, LogDatei, AnzahlNachrichten, SendeIntervall, ClientNummer, NummernListe) ->

  %Anstossen der Sendefunktion
  werkzeug:logging(LogDatei, "\n%%%%%%%%%%%%%%%%%%%%%\n% client: sendet Nachrichten\n%%%%%%%%%%%%%%%%%%%%%"),
  sendeNachricht(ServerPID, LogDatei, AnzahlNachrichten, SendeIntervall, ClientNummer, NummernListe),

  %Fehlernachricht provozieren
  ServerPID ! {getmsgid, self()},
  receive {nnr, Number} -> Number end,
  werkzeug:logging(LogDatei, integer_to_list(ClientNummer) ++ "-client : " ++ integer_to_list(Number) ++ "te_Nachricht um " ++ werkzeug:timeMilliSecond() ++ " vergessen zu senden!!!\n"),

  %Nachrichten abfragen
  werkzeug:logging(LogDatei, "%%%%%%%%%%%%%%%%%%%%%\n% client: fragt Nachrichten ab\n%%%%%%%%%%%%%%%%%%%%%\n\n"),
  empfangeNachrichten(LogDatei, ServerPID, NummernListe),

  %Neues Intervall für das Versenden der Nachrichten berechnen
  NeuerSendeIntervall = berechneIntervall(SendeIntervall),

  simulation(ServerPID, LogDatei, AnzahlNachrichten, NeuerSendeIntervall, ClientNummer, NummernListe)
.

%Funktion zum Abfragen der Nachrichten
empfangeNachrichten(LogDatei, ServerPID, NummernListe) ->
  ServerPID ! {getmessages, self()},
  receive
    {reply, Number, Nachricht, Terminated} ->

      %Prüfung ob die Nachricht die eigene ist
      NummernListe ! {finde, self(), Number},

      %falls eigene Nachricht, mit ****** Markieren
      receive
        nok -> NeueNachricht = Nachricht ++ "C In: " ++ werkzeug:timeMilliSecond();
        ok -> NeueNachricht = Nachricht ++ "C In: " ++ werkzeug:timeMilliSecond() ++ "******"
      end,

      %empfangene Nachricht Protokollieren
      werkzeug:logging(LogDatei, NeueNachricht ++ "\n"),

      %Prüfung ob es weitere unbekannte Nachrichten gibt
      case Terminated of
        true -> true;
        false -> empfangeNachrichten(LogDatei, ServerPID, NummernListe)
      end
  end
.

%Funktion zum senden von Nachrichten. Sendet Nachrichten
%%  solange bis alle gesendet sind
sendeNachricht(ServerPID, LogDatei, AnzahlNachrichten, SendeIntervall, ClientNummer, NummernListe) when AnzahlNachrichten > 0 ->
  ServerPID ! {getmsgid, self()},

  receive
    {nnr, Number} -> Number
  end,

  %Zu sendende Nachricht zusammenstellen
  {ok, Hostname} = inet:gethostname(),
  Nachricht = lists:concat(["\n", ClientNummer, "-client@", Hostname, "2", "19", " : ", Number, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()]),

  ServerPID ! {dropmessage, {Nachricht, Number}},
  werkzeug:logging(LogDatei, Nachricht ++ "\n"),

  NummernListe ! {add, Number},

  %Wartezeit zwischen den Nachrichten berechnen und so lange warten
  timer:sleep(round(SendeIntervall*1000)),

  sendeNachricht(ServerPID, LogDatei, AnzahlNachrichten-1, SendeIntervall, ClientNummer, NummernListe)
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

%Liste zum Speichern der Nummern der gesendeten
%%  Nachrichten und vergleicht diese auf Anfrage
liste(Liste) ->
  receive
    {add, Nummer} ->
      NeueListe = [Nummer | Liste],
      liste(NeueListe);

    {finde, Pid, Nummer} ->
      Test = lists:member(Nummer, Liste),
      if
        Test == true -> Pid ! ok;
        Test == false ->  Pid ! nok
      end,
      liste(Liste)
  end
.