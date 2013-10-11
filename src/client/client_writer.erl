-module(client_writer).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

deliver_messages(Server, ClientNumber, LogFile, MessageNumbers, Interval) ->
  NewMessageNumbers   = send_messages(Server, ClientNumber, Interval, MessageNumbers, tools:get_config_value(client, anzahl_nachrichten), LogFile),
  DummyMessageNumber  = get_next_number(Server),
  werkzeug:logging(LogFile, lists:concat([DummyMessageNumber, "te Nachricht um ", werkzeug:timeMilliSecond(), "| vergessen zu senden ******\n"])),
  NewMessageNumbers
.

send_messages(Server, ClientNumber, Interval, MessageNumbers, N, LogFile) when N > 0 ->
  timer:sleep(round(Interval * 1000)),
  MessageNumber = get_next_number(Server),
  Message       = generate_message(ClientNumber, MessageNumber),
  send_message(Server, Message),
  werkzeug:logging(LogFile, lists:concat(["Sending message: ", element(2, Message), "\n"])),
  send_messages(Server, ClientNumber, Interval, lists:append(MessageNumbers, [MessageNumber]), N - 1, LogFile)
;
send_messages(_, _, _, MessageNumbers, N, _) when N == 0 ->
  MessageNumbers
.

get_next_number(Server)->
  Server ! {getmsgid, self()},
  receive
    {nnr, N} -> N
  end
.

generate_message(ClientNumber, MessageNumber) ->
  {ok, Hostname} = inet:gethostname(),
  {MessageNumber, lists:concat([ClientNumber, "-client@", Hostname, " 1", "-08", " : ", MessageNumber, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()])}
.

send_message(Server, Message) ->
  {MessageNumber, Text} = Message,
  Server ! {dropmessage, {Text, MessageNumber}}
.