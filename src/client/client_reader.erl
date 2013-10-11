-module(client_reader).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile([debug_info, export_all]).

%Ruft neue Nachrichten vom Server ab
read_messages(Server, LogFile, MessageNumbers) ->
  Server ! {getmessages, self()},
  receive
    {reply, Number, Text, Terminate} ->

      Message = mark_message({Number, Text}, MessageNumbers),
      werkzeug:logging(LogFile, element(2, Message) ++ "\n"),

      if Terminate ->
        do_nothing;
      true ->
        read_messages(Server, LogFile, MessageNumbers)
      end
  end
.

%Markiert eigene Nachricht mit Sternen
mark_message(Message, MessageNumbers) ->
  {Number, Text}  = Message,
  TempMessage     = lists:concat([Text, " C In: " , werkzeug:timeMilliSecond(), "|"]),
  IsOwn           = lists:member(Number, MessageNumbers),
  if IsOwn ->
      {Number, lists:concat([TempMessage, "*******;"])};
    true ->
      {Number, TempMessage}
  end
.