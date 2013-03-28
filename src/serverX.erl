%% Copyright
-module(serverX).
-author("denisfleischhauer").

%% API
-export([start/0]).

start() ->
  {ok, ConfigListe} = file:consult("cfg/server.cfg"),
  {ok, LogDatei} = werkzeug:get_config_value(log_datei, ConfigListe),
  {ok, DLQGroesse} = werkzeug:get_config_value(dlq_limit, ConfigListe),

  NGen      = spawn(fun() -> naechsteNummer(1, LogDatei) end),
  register(ngen, NGen),

  HBQ       = spawn(fun() -> holdBackQueue([], LogDatei, DLQGroesse) end),
  register(hbq, HBQ),

  DLQ       = spawn(fun() -> deliveryQueue([], LogDatei, DLQGroesse, 1) end),
  register(dlq, DLQ),

%CMgr      = spawn(fun() -> clientManager(orddict:new()) end),
%register(cmgr, CMgr),

  ServerPID = spawn(fun() -> loop(LogDatei) end),
  register(wk,ServerPID),

  ServerPID
.

loop(LogDatei) ->
  receive
    {getmessages, ClientPID} ->
      hbq ! {getmsg, ClientPID},
      werkzeug:logging(LogDatei,"\n+Server: Nachrichten von "++werkzeug:to_String(ClientPID)++ " angefragt"),
      loop(LogDatei);

    {dropmessage, {Nachricht, Number}} ->
      hbq ! {add, Nachricht, Number},
      loop(LogDatei);

    {getmsgid, AbsenderPID} ->
      ngen ! {nnr, AbsenderPID},
      loop(LogDatei)
  end
.


naechsteNummer(AktuelleNummer, LogDatei) ->
  receive
    {nnr, AbsenderPID} ->
      AbsenderPID ! {nnr, AktuelleNummer},
      naechsteNummer(AktuelleNummer+1, LogDatei)
  end
.

holdBackQueue(Queue, LogDatei, DLQGroesse) ->
  receive
    {add, Nachricht, Number} ->
      NeueNachricht = lists:concat(["\n",Nachricht, " HBQ: ", werkzeug:timeMilliSecond()]),

      holdBackQueue([{Number, NeueNachricht} | Queue], LogDatei, DLQGroesse);
    {getmsg, AbsenderPID} ->
      %TODO
      werkzeug:logging(LogDatei,"\n-Server: in der HBQ"),
      AbsenderPID ! {reply,1,"\n1-client@MacBook-Pro206 : 10te Nachricht. C Out: 28.03 15:34:31,054|" ++ "HBQ In: " ++ werkzeug:timeMilliSecond(),true},
      holdBackQueue(Queue, LogDatei, DLQGroesse)
  end
.

deliveryQueue(Queue, LogDatei, DLQGroesse, LetzteMsgNr) ->
  receive
    {add, Nachricht, Number} ->
      NeueNachricht = lists:concat([Nachricht, " DLQ: ", werkzeug:timeMilliSecond()]),
      if length(Queue) > DLQGroesse ->
        werkzeug:delete_last(Queue),
        werkzeug:logging(LogDatei,"\n+Server: Letzte Nachricht aus DLQ geloescht")
      end,
      deliveryQueue([{Number, NeueNachricht} | Queue], LogDatei, DLQGroesse, LetzteMsgNr+1);
    {getmsg, AbsenderPID} ->
%TODO
%AbsenderPID ! {reply,1,"XXX",true},
      deliveryQueue(Queue, LogDatei, DLQGroesse, LetzteMsgNr);
    {getLetzteNr} ->
      hbq ! LetzteMsgNr
  end
.

%clientManager(ClientDict, []) ->
%NeueClientDict = ClientDi
%TODO
%.

%sendeAlleNachrichten(Queue, ClientPID) ->
%TODO
%.