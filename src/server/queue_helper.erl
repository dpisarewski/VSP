-module(queue_helper).
-compile([debug_info, export_all]).


queue(Queue) ->
	receive
    %Fügt eine neue Nachricht in die Queue ein
		{push, {Number, Text}} ->
			queue(lists:append(Queue, [{Number, Text}]));

    %Fügt mehrere Nachrichten ans Ende der Queue
		{append, Messages}->
			queue(lists:append(Queue, Messages));

    %Liefert alle Nachrichten aus der Queue
		{getall, Pid} ->
			Pid ! {messages, Queue},
      queue(Queue);

    %Ersetzt alle Nachrichten in der Queue
    {replace, Messages} ->
      queue(Messages);

    {shift, N, Max} ->
      if
        length(Queue) >= Max ->
          queue(lists:sublist(Queue, N + 1, Max - N + 1));
        true ->
          queue(Queue)
      end;

    {first, Pid} ->
      Pid ! {first, hd(Queue)},
      queue(Queue);

    {last, Pid} ->
      if Queue == [] ->
          Pid ! {last, none};
        true ->
          Pid ! {last, lists:last(Queue)}
      end,
      queue(Queue)
	end
.