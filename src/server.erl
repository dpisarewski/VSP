-module(server).
-compile(export_all).

start(Name) ->
  HBQ             = spawn(fun() -> queue_helper:queue([]) end),
  DQ              = spawn(fun() -> queue_helper:queue([]) end),
  Manager         = spawn(fun() -> manager:manager([HBQ, DQ]) end),
  Sender          = spawn(fun() -> sender:send_func(DQ) end),
  ClientManager   = spawn(fun() -> client_manager:loop([]) end),
  Server          = spawn(fun() -> loop([HBQ, DQ, Sender, Manager], 1, ClientManager) end),
  %register(Name, Server),
  {Server, ClientManager}
.

loop([HBQ, DQ, Sender, Manager], N, ClientManager) ->
  receive
    {getmsgid, Pid} ->
      Pid ! {nnr, N},
      loop([HBQ, DQ, Sender, Manager], N + 1, ClientManager);
    {getmessages, Pid} ->
      Sender ! {send_messages, Pid},
      loop([HBQ, DQ, Sender, Manager], N, ClientManager);
    {dropmessage, {Nachricht, Number}} ->
      HBQ ! {push, {Number, Nachricht}},
      Manager ! validate_queues,
      loop([HBQ, DQ, Sender, Manager], N, ClientManager)
  end
.