-module(queue_helper).
-compile([debug_info, export_all]).

%Löscht die erste N Nachrichten
shift(Queue, N, DQLimit) ->
  if
    length(Queue) >= DQLimit ->
      lists:sublist(Queue, N + 1, DQLimit - N + 1);
    true ->
      Queue
  end
.