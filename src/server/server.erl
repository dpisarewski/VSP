-module(server).
-compile([debug_info, export_all]).

start() ->

  Server = spawn(fun() ->
    ClientManager   = spawn_link(fun() -> client_manager:loop([]) end),
    tools:reregister(client_manager, ClientManager),
    %Löscht alte vorhadene Logdatei
    file:delete(tools:get_config_value(server, log_file)),
    tools:log(server, lists:concat(["Server started at: ", werkzeug:timeMilliSecond(), "| with PID ", werkzeug:to_String(self()), "\n"])),

    loop([], [], ClientManager, 1, no_timer)
  end),

  %Prüft, ob ein Prozess mit angegebenem Namen registriert ist, töten ihn und registriert neuen Prozess mit diesem Namen
  tools:reregister(tools:get_config_value(server, servername), Server),
  Server
.

loop(HBQ, DQ, ClientManager, N, Timer) ->
  %Startet einen Timer, der nach dem Ablauf eines Zeitintervals den Protzess beendet
  NewTimer = renew_timer(Timer, [ClientManager]),

  receive
    %Abfragen der eindeutigen Nachrichtennummer
    {getmsgid, Pid} ->
      tools:log(server, lists:concat(["Server: Messagenumber ", werkzeug:to_String(N), " sent to  ", werkzeug:to_String(Pid), "\n"])),
      Pid ! {nnr, N},
      loop(HBQ, DQ, ClientManager, N + 1, NewTimer);

    %Abfragen aller Nachrichten
    {getmessages, Pid} ->
      sender:deliver_messages(DQ, ClientManager, Pid),
      loop(HBQ, DQ, ClientManager, N, NewTimer);

    %Senden einer Nachricht
    {dropmessage, {Text, Number}} ->
      [NewHBQ, NewDQ]= queue_manager:push(HBQ, DQ, {Number, Text}),
      loop(NewHBQ, NewDQ, ClientManager, N, NewTimer)
  end
.

renew_timer(Timer, Processes) ->
  if
    Timer == no_timer ->
      false;
    true ->
      timer:cancel(Timer)
  end,
  {ok, NewTimer} = timer:apply_after(tools:get_config_value(server, latency) * 1000, ?MODULE, stop, [Processes]),
  NewTimer
.

stop(Processes) ->
  tools:log(server, "Terminating execution because no clients available\n"),
  [exit(Process, ok) || Process <- Processes]
.