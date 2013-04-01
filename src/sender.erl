-module(sender).
-compile(export_all).

send_func(DQ, ClientManager) ->
	receive
		{send_messages, Pid} ->
      tools:stdout("retrieving messages from DQ~n"),
			tools:synchronized_call(DQ, {getall, self()}, messages, fun(Messages)->
        %TODO Nachrichtennummer des Clients bekommen, Timestamp überprüfen
        send_messages(Messages, Pid)
      end),
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