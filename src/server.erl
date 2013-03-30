-module(server).
-compile(export_all).

start() ->
  HBQ             = spawn(fun() -> queue_helper:queue([]) end),
  DQ              = spawn(fun() -> queue_helper:queue([]) end),
  Manager         = spawn(fun() -> manager:manager([HBQ, DQ]) end),
  ClientManager   = spawn(fun() -> client_manager:loop([]) end),
  Sender          = spawn(fun() -> sender:send_func(DQ, ClientManager) end),
  Server          = spawn(fun() -> loop([HBQ, DQ, Sender, Manager, ClientManager], 1) end),
  tools:reregister(wk, Server),
  tools:reregister(hbq, HBQ),
  tools:reregister(dq, DQ),
  tools:reregister(manager, Manager),
  tools:reregister(sender, Sender),
  tools:reregister(client_manager, ClientManager),
  Server
.

loop([HBQ, DQ, Sender, Manager, ClientManager], N) ->
  receive
    {getmsgid, Pid} ->
      Pid ! {nnr, N},
      loop([HBQ, DQ, Sender, Manager, ClientManager], N + 1);
    {getmessages, Pid} ->
      Sender ! {send_messages, Pid},
      loop([HBQ, DQ, Sender, Manager, ClientManager], N);
    {dropmessage, {Nachricht, Number}} ->
      HBQ ! {push, {Number, Nachricht}},
      Manager ! validate_queues,
      loop([HBQ, DQ, Sender, Manager, ClientManager], N)
  end
.