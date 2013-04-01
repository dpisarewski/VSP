-module(queue_manager).
-compile(export_all).

manager([HBQ, DQ]) ->
  receive
    {push, Message} ->
      HBQ ! {push, Message},
      HBQ ! {getall, self(), []},
      manager([HBQ, DQ]);
    {messages, Messages, _} ->
      %check_for_gaps(Messages, [HBQ, DQ]),
      DQ ! {append, Messages},
      tools:stdout("added message to DQ~n"),
      manager([HBQ, DQ])
  end
.

check_for_gaps(Messages, [HBQ, DQ]) ->
  DQLimit = tools:get_config_value(dlq_limit),
  if length(Messages) >= DQLimit ->
      First = element(1, hd(Messages)),
      Next  = find_next_gap(Messages) + 1,
      ErrorMessage = make_error_message(First, First),
      DQ  ! {push, ErrorMessage},
      HBQ ! {remove, Next};
    true ->
      false
  end
.

make_error_message(First, Last) ->
  {Last, lists:concat(["***Fehlernachricht fuer Nachrichtennummern ", First, " bis ", Last, " um ", werkzeug:timeMilliSecond()])}
.

%Gibt die letzte Nachrichtennummer, die vor nächster Lücke steht
find_next_gap(Messages) ->
  lists:foldl(
    fun(Elem, Acc) ->
      if (Acc == 0) or (Elem == Acc + 1) -> Elem;
        true -> Acc
      end
    end, 0, Messages)
.