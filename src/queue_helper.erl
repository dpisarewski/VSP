-module(queue_helper).
-compile(export_all).


queue(Queue) ->
	receive
		{push, {Number, Text}} ->
			queue(lists:append(Queue, [{Number, Text}]));
		{append, Messages}->
			queue(lists:append(Queue, Messages));
		{getall, Pid} ->
			Pid ! {messages, Queue},
      queue(Queue);
    {remove, N} ->
      queue(lists:sublist(Queue, N, length(Queue)));
    {replace, Messages} ->
      queue(Messages);
		{pop, Pid} ->
      Keys = dict:fetch_keys(Queue),
      [FirstKey | _ ] = Keys,
			Value = dict:fetch(FirstKey, Queue),
			Pid ! {value, Value},
			queue(dict:erase(FirstKey, Queue))
	end
.