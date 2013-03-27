-module(sender).
-compile(export_all).

send_func(DQ) ->
	receive
		{send_messages, Pid} ->
			DQ ! {getall, self(), Pid},
			send_func(DQ);
		{values, Messages, Pid} ->
			send_messages(dict:to_list(Messages), Pid),
      send_func(DQ)
	end
.

send_messages([], _) ->
	[]
	;

send_messages(Messages, Pid) ->
	[Message | Tail] 	= Messages,
	{Number, Nachricht} = Message,
	Pid ! {reply, Number, Nachricht, Tail =/= []},
	send_messages(Tail, Pid)
.