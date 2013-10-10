-module(client_reader).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

start(Server, Writer, ClientNumber, LogFile) ->
  spawn(fun() -> loop(Server, Writer, ClientNumber, LogFile) end)
.

loop(Server, Writer, ClientNumber, LogFile)->
  receive
    read_messages ->
      read_messages(Server, Writer, LogFile)
  end,
  loop(Server, Writer, ClientNumber, LogFile)
.

read_messages(Server, Writer, LogFile) ->
  Server ! {getmessages, self()},
  receive
    {reply, Number, Text, Terminate} ->

      Message = mark_message({Number, Text},Writer),
      werkzeug:logging(LogFile, Message),

      if Terminate ->
        do_nothing;
      true ->
        read_messages(Server, Writer, LogFile)
      end
  end
.

check_if_own_message(Number, Writer) ->
  Writer ! {own_message, Number, self()},
  receive
    {own_message, IsOwn} -> IsOwn
  end
.

mark_message(Message, Writer) ->
  {Number, Text}  = Message,
  TempMessage     = lists:concat([Text, " C In: " , werkzeug:timeMilliSecond(), "|"]),
  IsOwn = check_if_own_message(Number, Writer),
  if IsOwn ->
      MarkMessage = {Number, lists:concat([TempMessage, "*******;"])};
    true ->
      MarkMessage = {Number, TempMessage}
  end
.