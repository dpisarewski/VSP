-module(server).
-compile([debug_info, export_all]).

start() ->
  Server = spawn(fun() ->
    HBQ             = spawn_link(fun() -> queue_helper:queue([]) end),
    DQ              = spawn_link(fun() -> queue_helper:queue([]) end),
    QueueManager    = spawn_link(fun() -> queue_manager:manager([HBQ, DQ]) end),
    ClientManager   = spawn_link(fun() -> client_manager:loop([]) end),

    tools:reregister(hbq, HBQ),
    tools:reregister(dq, DQ),
    tools:reregister(queue_manager, QueueManager),
    tools:reregister(client_manager, ClientManager),

    file:delete(tools:get_config_value(server, log_file)),
    tools:log(server, "Server Startzeit: " ++ werkzeug:timeMilliSecond() ++ "| mit PID " ++ werkzeug:to_String(self()) ++ "\n"),

    loop([HBQ, DQ, QueueManager, ClientManager], 1, no_timer)
  end),

  tools:reregister(tools:get_config_value(server, servername), Server),
  Server
.

loop([HBQ, DQ, Manager, ClientManager], N, Timer) ->
  %Startet einen Timer, der nach dem Ablauf eines Zeitintervals den Protzess beendet
  NewTimer = renew_timer(Timer),

  receive
    {getmsgid, Pid} ->
      tools:log(server, "Server: Nachrichtennummer " ++ werkzeug:to_String(N) ++ " an " ++ werkzeug:to_String(Pid) ++ " gesendet\n"),
      Pid ! {nnr, N},
      loop([HBQ, DQ, Manager, ClientManager], N + 1, NewTimer);

    {getmessages, Pid} ->
      sender:deliver_messages(DQ, ClientManager, Pid),
      loop([HBQ, DQ, Manager, ClientManager], N, NewTimer);

    {dropmessage, {Text, Number}} ->
      Manager ! {push, {Number, Text}},
      loop([HBQ, DQ, Manager, ClientManager], N, NewTimer)
  end
.

renew_timer(Timer) ->
  if
    Timer == no_timer ->
      false;
    true ->
      timer:cancel(Timer)
  end,
  {ok, NewTimer} = timer:exit_after(tools:get_config_value(server, latency) * 1000, "Terminating execution because no clients available\n"),
  NewTimer
.