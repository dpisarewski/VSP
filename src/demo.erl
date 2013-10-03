-module(demo).
-compile([debug_info, export_all]).

start(Server, N, Count) when N =< Count ->
  spawn(fun()-> client:start(Server, N) end),
  start(Server, N + 1, Count);
start(_, _, _) ->
  true.
start(N) ->
  Server = server:start(),
  start(Server, 1, N).
start() ->
  start(tools:get_config_value(demo, clients)).