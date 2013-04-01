-module(queue_helper).
-compile(export_all).


queue(Queue) ->
	receive
		{push, {Number, Text}} ->
			queue(lists:append(Queue, [{Number, Text}]));
		{append, Messages}->
			queue(lists:append(Queue, Messages));
		{getall, Pid, Arguments} ->
			Pid ! {messages, Queue, Arguments},
      queue(Queue);
    {remove, N} ->
      tools:stdout("removing messages from HBQ~n"),
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