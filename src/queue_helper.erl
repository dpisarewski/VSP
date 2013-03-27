-module(queue_helper).
-compile(export_all).


queue(Queue) ->
	receive
		{push, {Number, Text}} ->
			queue(dict:store(Number, Text, Queue));
		{append, Messages}->
			queue(dict:merge(fun(_, V, _) -> V end, Queue, Messages));
		{getall, Pid, Arguments} ->
			Pid ! {values, Queue, Arguments},
      queue(Queue);
		{pop, Pid} ->
      Keys = dict:fetch_keys(Queue),
      [FirstKey | _ ] = Keys,
			Value = dict:fetch(FirstKey, Queue),
			Pid ! {value, Value},
			queue(dict:erase(FirstKey, Queue))
	end
.