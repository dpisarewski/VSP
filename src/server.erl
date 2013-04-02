-module(server).
-compile(export_all).

start() ->
  HBQ             = spawn(fun() -> queue_helper:queue([]) end),
  DQ              = spawn(fun() -> queue_helper:queue([]) end),
  QueueManager    = spawn(fun() -> queue_manager:manager([HBQ, DQ]) end),
  ClientManager   = spawn(fun() -> client_manager:loop([]) end),
  Sender          = spawn(fun() -> sender:send_func(DQ, ClientManager) end),
  Server          = spawn(fun() -> loop([HBQ, DQ, Sender, QueueManager, ClientManager], 1) end),
  tools:reregister(wk, Server),
  tools:reregister(hbq, HBQ),
  tools:reregister(dq, DQ),
  tools:reregister(queue_manager, QueueManager),
  tools:reregister(client_manager, ClientManager),
  tools:reregister(sender, Sender),
  werkzeug:logging("server.log", "Server Startzeit: " ++ werkzeug:timeMilliSecond() ++ "| mit PID " ++ werkzeug:to_String(self()) ++ "~n"),
  Server
.

loop([HBQ, DQ, Sender, Manager, ClientManager], N) ->
  receive
    {getmsgid, Pid} ->
      Pid ! {nnr, N},
      werkzeug:logging("server.log", "Server: Nachrichtennummer " ++ werkzeug:to_String(N) ++ " an " ++ werkzeug:to_String(Pid) ++ " gesendet~n"),
      loop([HBQ, DQ, Sender, Manager, ClientManager], N + 1);
    {getmessages, Pid} ->
      Sender ! {send_messages, Pid},
      loop([HBQ, DQ, Sender, Manager, ClientManager], N);
    {dropmessage, {Text, Number}} ->
      Manager ! {push, {Number, Text}},
      loop([HBQ, DQ, Sender, Manager, ClientManager], N)
  end
.