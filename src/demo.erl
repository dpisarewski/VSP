-module(demo).
-compile([debug_info, export_all]).

start() ->
  server:start(),
  client:start(node()).