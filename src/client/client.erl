%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ServerNode) ->
  Clients = tools:get_config_value(client, clients),
  tools:times(Clients, fun(ClientNumber) ->
    start(ServerNode, ClientNumber)
  end)
.

start(ServerNode, ClientNumber) ->
  %Den Namen des Clientlogs generieren
  LogFile     = lists:concat(["log/", "client_", ClientNumber, "@", element(2, inet:gethostname()), ".log"]),
  %Löschen, falls die Datei schon vorhanden ist
  file:delete(LogFile),
  ServerName  = tools:get_config_value(client, servername),
  Server      = {ServerName, ServerNode},
  spawn(fun() ->
    {_,A2,A3} = now(),
    random:seed(A2, A3, ClientNumber),

    timer:apply_after(tools:get_config_value(client, lifetime) * 1000, ?MODULE, stop, [[self()]]),
    Interval   = tools:get_config_value(client, sendeintervall),
    werkzeug:logging(LogFile, lists:concat(["Client ", ClientNumber, " started\n"])),
    loop(Server, ClientNumber, LogFile, [], Interval)
  end)
.

loop(Server, ClientNumber, LogFile, MessageNumbers, Interval) ->
  SentMessageNumbers = client_writer:deliver_messages(Server, ClientNumber, LogFile, MessageNumbers, Interval),
  NewMessageNumbers = lists:append(MessageNumbers, SentMessageNumbers),
  client_reader:read_messages(Server, LogFile, NewMessageNumbers),
  NewInterval = calculateInterval(Interval),
  tools:stdout(lists:concat(["Interval changed from: ", Interval, " to: ", NewInterval, "\n"])),
  loop(Server, ClientNumber, LogFile, NewMessageNumbers, NewInterval)
.

calculateInterval(Intervall) ->
  %Zufallszahl erzeugen. -50% / +50%
  Sign    = round(random:uniform()) * 2 - 1,
  Change  = max(1.0, Intervall * 0.5),
  max(2.0, Intervall + Change * Sign)
.

stop(Processes) ->
  tools:stdout("Stopping client...\n"),
  [exit(Process, ok) || Process <- Processes]
.
