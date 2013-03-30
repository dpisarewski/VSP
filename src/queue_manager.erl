-module(queue_manager).
-compile(export_all).

manager([HBQ, DQ]) ->
  receive
    {push, Message} ->
      HBQ ! {push, Message},
      %TODO Überprüfung nach Lücken
      HBQ ! {getall, self(), []},
      manager([HBQ, DQ]);
    {messages, Messages, _} ->
      DQ ! {append, Messages},
      tools:stdout("added message to DQ~n"),
      manager([HBQ, DQ])
  end
.
