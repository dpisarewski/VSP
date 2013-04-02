-module(queue_manager).
-compile([debug_info, export_all]).

manager([HBQ, DQ]) ->
  receive
    {push, Message} ->
      %Hängt Information über die Eingangszeit an die Nachricht
      NewMessage = append_hbq_timestamp(Message),
      %Loggt die Nachricht
      tools:log(server, element(2, NewMessage) ++ "|-dropmessage\n"),
        %Fügt die neue Nachricht in die HBQ ein
        HBQ ! {push, NewMessage},
        %Fragt alle Nachrighten aus HBQ ab
        tools:synchronized_call(HBQ, {getall, self()}, messages, fun(Messages)->
          check_for_gaps(lists:sort(Messages), [HBQ, DQ])
        end),
        manager([HBQ, DQ])
  end
.

%Prüft, ob die HBQ voll ist, ob es eine Lücke gibt, und trägt Nachrichten aus HBQ in die DQ über
check_for_gaps(Messages, [HBQ, DQ]) ->
  DQLimit = tools:get_config_value(server, dlq_limit),
  if length(Messages) >= DQLimit / 2 ->
      fill_gap(Messages, DQ),
      transfer_messages(Messages, HBQ, DQ);
    true -> false
  end
.

%Trägt Nachrichten bis zur nächsten Lücke aus HBQ in die DQ
transfer_messages(Messages, HBQ, DQ) ->
  LastNumber = find_next_gap(Messages),
  DQ  ! {append, [Message || Message <- Messages, element(1, Message) =< LastNumber]},
  HBQ ! {replace, [Message || Message <- Messages, element(1, Message) > LastNumber]}
.

%Hängt Information über Verfügbarkeit einer Nachricht in der DQ an diese Nachricht
append_dq_timestamp(Message) ->
	{Id, Text} = Message,
	NewText = Text ++ "| DLQ In:" ++ werkzeug:timeMilliSecond(),
	{Id, NewText}
.

%Hängt Information über die Eingangszeit an eine Nachricht
append_hbq_timestamp(Message) ->
  {Number, Text} = Message,
  NewText = lists:concat([Text, "|(", Number, "); HBQ In: ", werkzeug:timeMilliSecond()]),
  {Number, NewText}
.

%Füllt die Lücke zwischen HBQ und DQ mit einer Fehlernachricht
fill_gap(Messages, DQ) ->
  %Holt die erste Nachrichtennummer aus HBQ
  FirstHBQ  = element(1, hd(Messages)),
  %Fragt alle Nachrichten aus DQ ab, um die letzte Nachrichtennummer zu bestimmen
  tools:synchronized_call(DQ, {getall, self()}, messages, fun(DQMessages)->
    %Bestimmt die letzte Nachrichtennummer in DQ
    LastDQ = if
      DQMessages =/= [] ->
        element(1, lists:last(DQMessages));
      true -> 0
    end,
    %Prüft, ob eine Lücke existiert, und füllt sie mit einer Fehlernachricht
    if
      FirstHBQ > LastDQ + 1 ->
        ErrorMessage  = make_error_message(LastDQ + 1, FirstHBQ - 1),
        DQ  ! {push, ErrorMessage};
      true -> false
    end
  end)
.

%Generiert eine Fehlernachricht
make_error_message(First, Last) ->
  {Last, lists:concat(["***Fehlernachricht fuer Nachrichtennummern ", First, " bis ", Last, " um ", werkzeug:timeMilliSecond()])}
.

%Gibt die letzte Nachrichtennummer, die vor nächster Lücke steht
find_next_gap(Messages) ->
  %Iteriert über Nachrichten und zählt Nachrichtennummer, bis eine Lücke gefunden wird
  lists:foldl(
    fun(Elem, Acc) ->
      {Number, _} = Elem,
      if (Acc == 0) or (Number == Acc + 1) -> Number;
        true -> Acc
      end
    end, 0, Messages)
.