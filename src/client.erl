%% Copyright
-module(client).
-author("denisfleischhauer").

%% API
-export([start/2]).

start(ServerPID, ClientNummer) ->
  {ok, ConfigListe} = file:consult("cfg/client.cfg"),
  {ok, LogDatei} = werkzeug:get_config_value(log_datei, ConfigListe),
  {ok, Sendeintervall} = werkzeug:get_config_value(intervall, ConfigListe),
  {ok, AnzahlSchritte} = werkzeug:get_config_value(anzahl_schritte, ConfigListe),
  {ok, LifeTime} = werkzeug:get_config_value(life_time, ConfigListe),

  NachrichtenSammler = spawn(fun() -> nachrichtenNummern([],[1],LogDatei) end),
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


simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer, NachrichtenSammler) ->

  sendeNachricht(ServerPID, LogDatei, AnzahlSchritte, Sendeintervall, ClientNummer, NachrichtenSammler),

  ServerPID ! {getmsgid, self()},
  receive {nnr, Number} -> Number end,

  werkzeug:logging(LogDatei, "\n" ++ integer_to_list(ClientNummer) ++ "-client : " ++ integer_to_list(Number) ++ "te_Nachricht um " ++ werkzeug:timeMilliSecond() ++ " vergessen zu senden!!!"),

  frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler),

  simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer, NachrichtenSammler)
.

frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler) ->
  ServerPID ! {getmessages, self()},
  receive
    {reply, Number, Nachricht, Terminated} ->
      NachrichtenSammler ! {add, empfangene, Number},

      NachrichtenSammler ! {istEigeneNachricht, Number, self()},
      receive
        ok -> NeueNachricht = Nachricht ++ "C In: " ++ werkzeug:timeMilliSecond() ++ "******";
        nok -> NeueNachricht = Nachricht ++ "C In: " ++ werkzeug:timeMilliSecond()
      end,

      werkzeug:logging(LogDatei, NeueNachricht),

      case Terminated of
        true -> {reply, Number, NeueNachricht, Terminated};
        false -> frageNeueNachrichtenAb(LogDatei, ServerPID, NachrichtenSammler)
      end
  end
.

sendeNachricht(ServerPID, LogDatei, AnzahlSchritte, Sendeintervall, ClientNummer, NachrichtenSammler) when AnzahlSchritte > 0 ->
  ServerPID ! {getmsgid, self()},

  receive {nnr, Number} -> Number end,

  {ok, Hostname} = inet:gethostname(),
  Nachricht = lists:concat(["\n", ClientNummer, "-client@", Hostname, "2", "06", " : ", Number, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()]),

  ServerPID ! {dropmessage, {Nachricht, Number}},
  werkzeug:logging(LogDatei, Nachricht),

  NachrichtenSammler ! {add, gesendete, Number},
  timer:sleep(berechneIntervall(Sendeintervall)),

  sendeNachricht(ServerPID, LogDatei, AnzahlSchritte-1, Sendeintervall, ClientNummer, NachrichtenSammler);
sendeNachricht(ServerPID, _, _, _, _, _) -> ServerPID
.

berechneIntervall(Intervall) ->
  Rand = random:uniform(2),
  if
    Rand == 1 -> TIntervall = round(Intervall - (Intervall*0.5));
    Rand == 2 -> TIntervall = round(Intervall + (Intervall*0.5))
  end,

  if
    TIntervall < 1000 -> NeuerIntervall = 3000;
    true -> NeuerIntervall = TIntervall
  end,

  NeuerIntervall
.

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

listFind ( _, [] ) -> false;
listFind ( Element, [ Item | ListTail ] ) ->
  case ( Item == Element ) of
    true    ->  true;
    false   ->  listFind(Element, ListTail)
  end.