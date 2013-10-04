-module(client_writer).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(Server, ClientNumber, LogFile) ->
  spawn(fun() -> loop(Server, ClientNumber, LogFile, [], tools:get_config_value(client, intervall)) end)
.

loop(Server, ClientNumber, LogFile, MessageList, Interval)->
  NewInterval = calculateInterval(Interval),
  receive
    send_new_message ->
      NewMessageNumbers   = send_messages(Server, ClientNumber, NewInterval, 5),
      DummyMessageNumber  = get_next_number(Server),
      werkzeug:logging(LogFile, lists:concat(DummyMessageNumber, "te Nachricht um ", werkzeug:timeMilliSecond(), "| vergessen zu senden ******")),
      loop(Server, ClientNumber, LogFile, lists:append(MessageList, NewMessageNumbers), NewInterval);

    %Prüft, ob angegebene Nachrichtennummer vom eigenen Redakteur generiert wurde
    {own_message, N, Pid} ->
      Pid ! {own_message, lists:member(N, MessageList)},
      loop(Server, ClientNumber, LogFile, MessageList, NewInterval)
  end
.

send_messages(Server, ClientNumber, Interval, MessageNumbers, N) when N > 0 ->
  timer:sleep(round(Interval * 1000)),
  MessageNumber = get_next_number(Server),
  Message       = generate_message(ClientNumber, MessageNumber),
  send_message(Server, Message),
  send_messages(Server, ClientNumber, Interval, lists:concat(MessageNumbers, [MessageNumber]), N - 1)
;
send_messages(Server, ClientNumber, Interval, MessageNumbers, N) when N == 0 ->
  do_nothing
.

get_next_number(Server)->
  Server ! {getmsgid, self()},
  receive
    {nnr, N} ->
      N
  end
.

generate_message(ClientNumber, MessageNumber) ->
  {ok, Hostname} = inet:gethostname(),
  {MessageNumber, lists:concat([ClientNumber, "-client@", Hostname, " 1", "-08", " : ", MessageNumber, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()])}
.

send_message(Server, Message) ->
  {MessageNumber, Text} = Message,
  Server ! {dropmessage, Text, MessageNumber}
.

calculateInterval(Intervall) ->
  %Zufallszahl erzeugen. -50% / +50%
  Sign    = round(random:uniform()) * 2 - 1,
  Change  = max(1.0, Intervall * 0.5),
  max(2.0, Intervall + Change * Sign)
.