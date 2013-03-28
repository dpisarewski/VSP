-module(manager).
-compile(export_all).

manager([HBQ, DQ])->
  receive
    validate_queues->
      tools:stdout("validate queues~n"),
      HBQ ! {getall, self(), []},
      manager([HBQ, DQ]);
    {values, Messages, _}->
      DQ ! {append, Messages},
      tools:stdout("added message to DQ~n"),
      manager([HBQ, DQ])
  end
.
