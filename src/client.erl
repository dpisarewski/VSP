%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ServerNode, ClientNumber) ->
  ServerName  = tools:get_config_value(client, servername),
  Server      = {ServerName, ServerNode},
  spawn(fun() -> loop(Server, ClientNumber, 1) end)
.

loop(Server, ClientNumber, MessageNumber) ->
  Message = generate_message(ClientNumber, MessageNumber),
  send_message(Server, Message),
  loop(Server, ClientNumber, MessageNumber + 1)
.

send_message(Server, Message) ->
  stub
.

generate_message(ClientNumber, MessageNumber) ->
  {ok, Hostname} = inet:gethostname(),
  lists:concat([ClientNumber, "-client@", Hostname, " 1", "-08", " : ", MessageNumber, "te Nachricht. C Out: ", werkzeug:timeMilliSecond()])
.