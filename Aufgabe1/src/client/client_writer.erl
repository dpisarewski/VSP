-module(client_writer).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

%Generiert und sendet neue Nachrichten an den Server
deliver_messages(Server, ClientNumber, LogFile, MessageNumbers, Interval) ->
  %Sendet 5 Message und bekommt eine Liste mit Nachrichtennummmern zuruck
  NewMessageNumbers   = send_messages(Server, ClientNumber, Interval, MessageNumbers, tools:get_config_value(client, anzahl_nachrichten), LogFile),
  %Bekommt eine neue Nachrichtennummer und vergisst die Nachricht zu senden
  DummyMessageNumber  = get_next_number(Server),
  werkzeug:logging(LogFile, lists:concat([DummyMessageNumber, "te Nachricht um ", werkzeug:timeMilliSecond(), "| vergessen zu senden ******\n"])),
  NewMessageNumbers
.

%Sendet 5 Message und liefert eine Liste mit Nachrichtennummmern zuruck
send_messages(Server, ClientNumber, Interval, MessageNumbers, N, LogFile) when N > 0 ->
  %Bekommt eine neue Nachrichtennummer
  MessageNumber = get_next_number(Server),
  werkzeug:logging(LogFile, lists:concat(["Received message number: ", MessageNumber, "\n"])),
  %Generiert eine neue Nachricht
  Message       = generate_message(ClientNumber, MessageNumber),
  %Wartet *** Sekunden
  timer:sleep(round(Interval * 1000)),
  %Sendet die neue Nachricht
  send_message(Server, Message),
  werkzeug:logging(LogFile, lists:concat(["Sending message: ", element(2, Message), "\n"])),
  send_messages(Server, ClientNumber, Interval, lists:append(MessageNumbers, [MessageNumber]), N - 1, LogFile)
;
send_messages(_, _, _, MessageNumbers, N, _) when N == 0 ->
  MessageNumbers
.

%Bekommt vom Server eine neue Nachrichtennummer
get_next_number(Server)->
  Server ! {getmsgid, self()},
  receive
    {nid, N} -> N
  end
.

%Generiert neue Nachricht
generate_message(ClientNumber, MessageNumber) ->
  {ok, Hostname} = inet:gethostname(),
  {MessageNumber, lists:concat([ClientNumber, "-client@", Hostname, " 1", "-08", " : ", MessageNumber, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()])}
.

%Sendet neue Nachricht
send_message(Server, Message) ->
  {MessageNumber, Text} = Message,
  Server ! {dropmessage, {Text, MessageNumber}}
.