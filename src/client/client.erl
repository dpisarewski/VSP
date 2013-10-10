%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ServerNode, ClientNumber) ->
  LogFile     = lists:concat(["log/", "client_", ClientNumber, "@", element(2, inet:gethostname()), ".log"]),
  file:delete(LogFile),
  ServerName  = tools:get_config_value(client, servername),
  Server      = {ServerName, ServerNode},
  spawn(fun() ->
    Writer      = client_writer:start(Server, self(), ClientNumber, LogFile),
    Reader      = client_reader:start(Server, self(), Writer, ClientNumber, LogFile),
    werkzeug:logging(LogFile, lists:concat(["Client ", ClientNumber, " started\n"])),
    timer:apply_after(tools:get_config_value(client, lifetime) * 1000, ?MODULE, stop, [[self(), Writer, Reader]]),
    loop(Server, Writer, Reader, ClientNumber, LogFile)
end)
.

loop(Server, Writer, Reader, ClientNumber, LogFile) ->
  Writer ! send_messages,
  receive
    ok -> ok
  end,
  Reader ! read_messages,
  receive
    ok -> ok
  end,
  loop(Server, Writer, Reader, ClientNumber, LogFile)
.

stop(Processes) ->
  tools:stdout("Stopping client...\n"),
  [exit(Process, ok) || Process <- Processes]
.
