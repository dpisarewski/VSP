%% Copyright
-module(client).
-author("denisfleischhauer").

%% API
-export([start/2]).

%Startfunktion. Wird als allererstes gestartet
%Es werden die Konfigurationsdatei eingelesen
%und Parameter von dem Client-Prozess gesetzt
start(ServerPID, ClientNummer) ->
%Datei in der die Einstellungen Hinterlegt
%%  sind
  {ok, ConfigListe} = file:consult("client.cfg"),
%Der Pfad und Name der Datei für Logging
  {ok, LogDatei} = werkzeug:get_config_value(log_datei, ConfigListe),
%Intervall mit dem die Nachrichten versendet werden
  {ok, Sendeintervall} = werkzeug:get_config_value(intervall, ConfigListe),
%Anzahl der Nachrichten die hintereinander versendet werden
  {ok, AnzahlSchritte} = werkzeug:get_config_value(anzahl_schritte, ConfigListe),
%Zeit die der Client "lebt"
  {ok, LifeTime} = werkzeug:get_config_value(life_time, ConfigListe),

%Prozess der gesendete und empfangene Nachrichten speichert
  NachrichtenSammler = spawn(fun() -> nachrichtenNummern([],[1],LogDatei) end),
%Starten des Clients und anschliessendes senden des Stopsignals um den Timer zu starten
  ClientPID = spawn(fun() -> simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer, NachrichtenSammler) end),
  ClientPID ! stop,
  receive
    stop -> stop
  after (LifeTime) ->
    werkzeug:logging(LogDatei,"\n-Client: beendet"),
    exit(ClientPID, kill)
  end,
  ClientPID
.

%Schleife die der Client immerwieder durchläuft
%%  bis der Timer abläeft
simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer, NachrichtenSammler) ->

%Anstossen der Sendefunktion
  werkzeug:logging(LogDatei, "\n%%%%%%%%%%%%%%%%%%%%%\n% client: sendet Nachrichten\n%%%%%%%%%%%%%%%%%%%%%"),
  sendeNachricht(ServerPID, LogDatei, AnzahlSchritte, Sendeintervall, ClientNummer, NachrichtenSammler),

%Fehlernachricht provozieren
  ServerPID ! {getmsgid, self()},
  receive {nnr, Number1} -> Number1 end,
  werkzeug:logging(LogDatei, "\n" ++ integer_to_list(ClientNummer) ++ "-client : " ++ integer_to_list(Number1) ++ "te_Nachricht um " ++ werkzeug:timeMilliSecond() ++ " vergessen zu senden!!!"),

%Nachrichten abfragen
  werkzeug:logging(LogDatei, "\n%%%%%%%%%%%%%%%%%%%%%\n% client: fragt Nachrichten ab\n%%%%%%%%%%%%%%%%%%%%%"),
  frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler),

%Neues Intervall für das Versenden der Nachrichten berechnen
  berechneIntervall(Sendeintervall),

  simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer, NachrichtenSammler)
.

%Funktion zum Abfragen der Nachrichten
frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler) ->
  ServerPID ! {getmessages, self()},
  receive
    {reply, Number, Nachricht, Terminated} ->

%Nummer der empfangenen Nachricht speichern
      NachrichtenSammler ! {add, empfangene, Number},
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
sendeNachricht(ServerPID, LogDatei, AnzahlSchritte, Sendeintervall, ClientNummer, NachrichtenSammler) when AnzahlSchritte > 0 ->
  ServerPID ! {getmsgid, self()},

  receive
    {nnr, Number} -> Number
  end,

%Zusendende Nachricht zusammenstellen
  {ok, Hostname} = inet:gethostname(),
  Nachricht = lists:concat(["\n", ClientNummer, "-client@", Hostname, "2", "06", " : ", Number, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()]),

  ServerPID ! {dropmessage, {Nachricht, Number}},
  werkzeug:logging(LogDatei, Nachricht),

  NachrichtenSammler ! {add, gesendete, Number},

%wartezeit Zwischen den Nachrichten berechnen und solange warten
  timer:sleep(Sendeintervall),

  sendeNachricht(ServerPID, LogDatei, AnzahlSchritte-1, Sendeintervall, ClientNummer, NachrichtenSammler);
sendeNachricht(ServerPID, _, _, _, _, _) -> ServerPID
.

%Funktion zur Berechnung des Intervalls zwischen
%%  dem Versenden der Nachrichten
berechneIntervall(Intervall) ->
  %Zufallszahl erzeugen. 1: -50% 2: +50%
  Sign = round(random:uniform() - 0.5),
  Change = max(1.0, Intervall * 0.5),
  max(1.0, Intervall + Change * Sign)
.

%Speichert die Nummern der gesendeten und empfangenen
%%  Nachrichten und vergleicht diese auf Anfrage
nachrichtenNummern(Gesendete, Empfangene, LogDatei) ->
  receive
    {add, gesendete, Nummer} ->
      NeueGesendete = [Nummer | Gesendete],
      nachrichtenNummern(NeueGesendete, Empfangene, LogDatei);

    {add, empfangene, Nummer} ->
      NeueEmpfangene = [Nummer | Empfangene],
      nachrichtenNummern(Gesendete, NeueEmpfangene, LogDatei);

    {istEigeneNachricht, Nummer, Absender} ->
      Flag = listFind(Nummer, Gesendete),
      if
        Flag == true -> Absender ! ok;
        Flag == false ->  Absender ! nok
      end,
      nachrichtenNummern(Gesendete, Empfangene, LogDatei)
  end
.

%Funktion zum Suchen der Elemente in einer Liste
listFind ( _, [] ) -> false;
listFind ( Element, [ Item | ListTail ] ) ->
  case ( Item == Element ) of
    true    ->  true;
    false   ->  listFind(Element, ListTail)
  end
.