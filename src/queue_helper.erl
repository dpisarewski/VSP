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
      queue(Messages)
	end
.