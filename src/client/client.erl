%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ServerNode, ClientNumber) ->
  LogFile     = lists:concat(["client_", ClientNumber, "@", element(2, inet:gethostname()), ".log"]),
  ServerName  = tools:get_config_value(client, servername),
  Server      = {ServerName, ServerNode},
  Writer      = spawn(fun()->client_writer:start(Server, ClientNumber, LogFile) end),
  Reader      = spawn(fun()->client_reader:start(Server, ClientNumber, LogFile) end),
  spawn(fun() -> loop(Server, ClientNumber, LogFile) end)
.

loop(Server, ClientNumber, LogFile) ->

  loop(Server, ClientNumber, LogFile)
.
