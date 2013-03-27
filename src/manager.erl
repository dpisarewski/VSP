-module(manager).
-compile(export_all).

manager([HBQ, DQ])->
  receive
    validate_queues->
      tools:stdout("validate queues~n"),
      HBQ ! {getall, self(), []},
      manager([HBQ, DQ]);
    {values, Messages, _}->
      tools:stdout("received messages from HBQ~n"),
      DQ ! {append, Messages},
      manager([HBQ, DQ])
  end
.
