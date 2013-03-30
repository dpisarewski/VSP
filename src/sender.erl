-module(sender).
-compile(export_all).

send_func(DQ, ClientManager) ->
	receive
		{send_messages, Pid} ->
      tools:stdout("retrieving messages from DQ~n"),
			DQ ! {getall, self(), Pid},
			send_func(DQ, ClientManager);
		{values, Messages, Pid} ->
			send_messages(Messages, Pid),
      send_func(DQ, ClientManager)
	end
.

send_messages([], _) ->
	[]
	;

send_messages(Messages, Pid) ->
	[Message | Tail] 	= Messages,
	{Number, Nachricht} = Message,
  tools:stdout("sending message " ++ werkzeug:to_String(Number) ++ " to client " ++ pid_to_list(Pid)),
	Pid ! {reply, Number, Nachricht, Tail == []}
	%send_messages(Tail, Pid)
.