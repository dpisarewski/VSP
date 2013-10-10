%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ServerNode, ClientNumber) ->
  LogFile     = lists:concat(["client_", ClientNumber, "@", element(2, inet:gethostname()), ".log"]),
  ServerName  = tools:get_config_value(client, servername),
  Server      = {ServerName, ServerNode},
  Writer      = spawn_link(fun()->client_writer:start(Server, ClientNumber, LogFile) end),
  Reader      = spawn_link(fun()->client_reader:start(Server, Writer, ClientNumber, LogFile) end),
  spawn(fun() ->
    timer:exit_after(tools:get_config_value(client, lifetime) * 1000, "Client ended"),
    loop(Server, Writer, Reader, ClientNumber, LogFile)
  end)
.

loop(Server, Writer, Reader, ClientNumber, LogFile) ->
  Writer ! send_messages,
  Reader ! read_messages,
  loop(Server, Writer, Reader, ClientNumber, LogFile)
.
