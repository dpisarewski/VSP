-module(queue_helper).
-compile([debug_info, export_all]).


queue(Queue) ->
	receive
		{push, {Number, Text}} ->
			queue(lists:append(Queue, [{Number, Text}]));
		{append, Messages}->
			queue(lists:append(Queue, Messages));
		{getall, Pid} ->
			Pid ! {messages, Queue},
      queue(Queue);
    {replace, Messages} ->
      queue(Messages)
	end
.