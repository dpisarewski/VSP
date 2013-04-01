-module(queue_manager).
-compile(export_all).

manager([HBQ, DQ]) ->
  receive
    {push, Message} ->
      HBQ ! {push, Message},
      tools:synchronized_call(HBQ, {getall, self()}, messages, fun(Messages)-> check_for_gaps(Messages, [HBQ, DQ]) end),
      manager([HBQ, DQ])
  end
.

check_for_gaps(Messages, [HBQ, DQ]) ->
  DQLimit = tools:get_config_value(dlq_limit),
  if length(Messages) >= DQLimit / 2 ->
      fill_gap(Messages, DQ),
      transfer_messages(Messages, HBQ, DQ);
    true -> false
  end
.

transfer_messages(Messages, HBQ, DQ) ->
  LastNumber = find_next_gap(Messages),
  DQ  ! {append, [Message || Message <- Messages, element(1, Message) =< LastNumber]},
  HBQ ! {replace, [Message || Message <- Messages, element(1, Message) > LastNumber]}
.

fill_gap(Messages, DQ) ->
  FirstHBQ  = element(1, hd(Messages)),
  tools:synchronized_call(DQ, {getall, self()}, messages, fun(DQMessages)->
    LastDQ = if DQMessages =/= [] ->
      element(1, lists:last(DQMessages));
      true -> 0
    end,
    if FirstHBQ > LastDQ + 1 ->
      ErrorMessage  = make_error_message(LastDQ + 1, FirstHBQ - 1),
      DQ  ! {push, ErrorMessage};
      true -> false
    end
  end)
.

make_error_message(First, Last) ->
  {Last, lists:concat(["***Fehlernachricht fuer Nachrichtennummern ", First, " bis ", Last, " um ", werkzeug:timeMilliSecond()])}
.

%Gibt die letzte Nachrichtennummer, die vor nächster Lücke steht
find_next_gap(Messages) ->
  lists:foldl(
    fun(Elem, Acc) ->
      {Number, _} = Elem,
      if (Acc == 0) or (Number == Acc + 1) -> Number;
        true -> Acc
      end
    end, 0, Messages)
.