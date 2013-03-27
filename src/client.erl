%% Copyright
-module(client).
-author("denisfleischhauer").
%%TEST
%% API
-export([start/2]).

start(ServerPID, ClientNummer) ->
  {ok, ConfigListe} = file:consult("cfg/client.cfg"),
  {ok, LogDatei} = werkzeug:get_config_value(log_datei, ConfigListe),
  {ok, Sendeintervall} = werkzeug:get_config_value(intervall, ConfigListe),
  {ok, AnzahlSchritte} = werkzeug:get_config_value(anzahl_schritte, ConfigListe),
  {ok, LifeTime} = werkzeug:get_config_value(life_time, ConfigListe),

  NachrichtenSammler = spawn(fun() -> nachrichtenNummern([],[]) end),
  register(nSammer, NachrichtenSammler),
  ClientPID = spawn(fun() -> simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer) end),
  ClientPID ! stop,
  receive
    stop -> stop
  after (LifeTime) ->
    werkzeug:logging(LogDatei,"-Client: beendet"),
    exit(ClientPID, kill)
  end,
  ClientPID
.


simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer) ->

  sendeNachricht(ServerPID, LogDatei, AnzahlSchritte, Sendeintervall, ClientNummer),

  ServerPID ! {getmsgid, self()},
  receive {nnr, Number} -> Number end,

  werkzeug:logging(LogDatei, integer_to_list(Number) ++ "te_Nachricht um " ++ werkzeug:timeMilliSecond() ++ " vergessen zu senden ******\n"),

  frageNeueNachrichtenAb(ServerPID, 1, LogDatei, false),

  simulation(AnzahlSchritte, ServerPID, LogDatei, Sendeintervall, ClientNummer)
.

frageNeueNachrichtenAb(ServerPID, LetzteNachricht, LogDatei, NichtTerminated) when NichtTerminated == false ->
  ServerPID ! {getmessages, LetzteNachricht},
  werkzeug:logging(LogDatei,"-Client: " ++ integer_to_list(LetzteNachricht) ++ "te_Nachricht von Server angefragt\n"),

  receive
    {reply, Number, Nachricht, Terminated} ->
      nSamler ! {istEigeneNachricht, Number, self()},
      receive Flag -> Flag end,

      if
        Flag == ok ->
          NeueNachricht = lists:concat(Nachricht,"******");
        true ->
          NeueNachricht = Nachricht
      end,
      werkzeug:logging(LogDatei,"-Client: " ++ integer_to_list(Number) ++ "te_Nachricht um "
        ++ werkzeug:timeMilliSecond() ++ " mit Inhalt: " ++ NeueNachricht ++ " von Server empfangen\n"),
      frageNeueNachrichtenAb(ServerPID, LetzteNachricht, LogDatei, Terminated)
  end,

  frageNeueNachrichtenAb(ServerPID, LetzteNachricht, LogDatei, Terminated);
  frageNeueNachrichtenAb(ServerPID, _, _, _) -> ServerPID ! done
.

sendeNachricht(ServerPID, LogDatei, AnzahlSchritte, Sendeintervall, ClientNummer) when AnzahlSchritte > 0 ->
  ServerPID ! {getmsgid, self()},

  receive
    {nnr, Number} -> Number
  end,

  {ok, Hostname} = inet:gethostname(),
  Nachricht = lists:concat([ClientNummer, "-client@", Hostname, "2", "06", " : ", Number, "te Nachricht. C Out: ", werkzeug:timeMilliSecond(), "gesendet\n"]),

  ServerPID ! {dropmessage, {Nachricht, Number}},
  werkzeug:logging(LogDatei, Nachricht),

  timer:sleep(berechneIntervall(Sendeintervall)),

  sendeNachricht(ServerPID, LogDatei, AnzahlSchritte-1, Sendeintervall, ClientNummer);
  sendeNachricht(ServerPID, _, _, _, _) -> ServerPID ! done
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

nachrichtenNummern(Gesendete, Empfangene) ->
  receive
    {add, gesendete, Nummer} ->
      NeueGesendete = [ Nummer | Gesendete],
      nachrichtenNummern(NeueGesendete, Empfangene);

    {add, empfangene, Nummer} ->
      NeueEmpfangene = [ Nummer | Empfangene],
      nachrichtenNummern(Gesendete, NeueEmpfangene);

    {getLetzteNummerEmpfangene, Absender} ->
      [Head | _] = Empfangene,
      Absender ! Head,
      nachrichtenNummern(Gesendete, Empfangene);

    {istEigeneNachricht, Nummer, Absender} ->
      Flag = lists:keyfind(Nummer,1,Gesendete),
      if
        Flag == false -> Absender ! nok;
        true ->  Absender ! ok
      end,
      nachrichtenNummern(Gesendete, Empfangene)
  end
.  
