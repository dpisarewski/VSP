-module(queue_helper).
-compile([debug_info, export_all]).


shift(Queue, N, Max) ->
  if
    length(Queue) >= Max ->
      lists:sublist(Queue, N + 1, Max - N + 1);
    true ->
      Queue
  end
.