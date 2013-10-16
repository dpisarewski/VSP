%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ServerNode) ->
  connect_to_node(ServerNode),
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
  %Servernamen aus der Konfigaration laden
  ServerName  = tools:get_config_value(client, servername),
  %Server Pid generieren
  Server      = {ServerName, ServerNode},

  spawn(fun() ->
    %Zufallszahlengenerator initialisieren
    {_,A2,A3} = now(),
    random:seed(A2, A3, ClientNumber),

    %Timer starten, der den Client beendet
    timer:apply_after(tools:get_config_value(client, lifetime) * 1000, ?MODULE, stop, [[self()], LogFile]),
    %Sendeinterval aus der Konfiguration laden
    Interval   = tools:get_config_value(client, sendeintervall),
    werkzeug:logging(LogFile, lists:concat(["Client ", ClientNumber, " started\n"])),

    loop(Server, ClientNumber, LogFile, [], Interval)
  end)
.

connect_to_node(ServerNode) ->
  Result = net_adm:ping(ServerNode),
  if Result == pong ->
    tools:stdout(lists:concat(["Connected to node: ", ServerNode]));
    true ->
      ErrorMessage = lists:concat(["ERROR: Could not connect to node: ", ServerNode]),
      tools:stdout(ErrorMessage),
      exit(ErrorMessage)
  end
.

loop(Server, ClientNumber, LogFile, MessageNumbers, Interval) ->
  %Nachrichten generieren  und senden
  SentMessageNumbers  = client_writer:deliver_messages(Server, ClientNumber, LogFile, MessageNumbers, Interval),
  %Nummern der neuen Nachrichten speichern
  NewMessageNumbers   = lists:append(MessageNumbers, SentMessageNumbers),
  %Nachrichten vom Server abrufen
  client_reader:read_messages(Server, LogFile, NewMessageNumbers),
  %Neuen Sendeinterval berechnen
  NewInterval = calculateInterval(Interval),
  tools:stdout(lists:concat(["Interval changed from: ", Interval, " to: ", NewInterval, "\n"])),

  loop(Server, ClientNumber, LogFile, NewMessageNumbers, NewInterval)
.

calculateInterval(Interval) ->
  %Zufallszahl erzeugen. -50% / +50%
  Sign    = round(random:uniform()) * 2 - 1,
  Change  = max(1.0, Interval * 0.5),
  max(2.0, Interval + Change * Sign)
.

stop(Processes, LogFile) ->
  werkzeug:logging(LogFile, "Client stopped...\n"),
  [exit(Process, ok) || Process <- Processes]
.
