%% Copyright
-module(client).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(ClientNumber) ->
  spawn(loop)
.

loop(ClientNumber, MessageNumber) ->
  Message = generate_message(ClientNumber, MessageNumber),
  send_message(Message)
.

send_message(Message) ->
  stub
.

generate_message(ClientNumber, MessageNumber) ->
  stub
.