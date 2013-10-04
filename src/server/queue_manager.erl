-module(queue_manager).
-compile([debug_info, export_all]).

manager([HBQ, DQ]) ->
  receive
    {push, Message} ->
      %Hängt Information über die Eingangszeit an die Nachricht
      NewMessage = append_hbq_timestamp(Message),
      %Loggt die Nachricht
      tools:log(server, element(2, NewMessage) ++ "|-dropmessage\n"),
      %Prüfen, ob die Nachricht noch nicht abgearbeitet wurde und in HBQ einfügen
      check_order(HBQ, DQ, NewMessage),
      %Fragt alle Nachrighten aus HBQ ab
      tools:synchronized_call(HBQ, {getall, self()}, messages, fun(Messages)->
        check_for_gaps(lists:sort(Messages), [HBQ, DQ])
      end),
      manager([HBQ, DQ])
  end
.

%Fügt die empfangene Nachricht in die HBQ ein, wenn sie kleinere Nummer als die größte Nummer in DQ hat
check_order(HBQ, DQ, Message) ->
  LastDQ    = get_last_dq(DQ),
  if LastDQ < element(1, Message) ->
  %Fügt die neue Nachricht in die HBQ ein
    HBQ ! {push, Message};
    true -> false
  end
.

%Prüft, ob die HBQ voll ist, ob es eine Lücke gibt, und trägt Nachrichten aus HBQ in die DQ über
check_for_gaps(Messages, [HBQ, DQ]) ->
  DQLimit = tools:get_config_value(server, dlq_limit),
  if length(Messages) >= (DQLimit / 2) ->
      fill_gap(Messages, DQ);
    true -> false
  end,
  if Messages =/= [] ->
    transfer_messages(Messages, HBQ, DQ);
    true -> false
  end
.

%Trägt Nachrichten bis zur nächsten Lücke aus HBQ in die DQ
transfer_messages(Messages, HBQ, DQ) ->
  LastDQ      = get_last_dq(DQ),
  FirstHBQ    = element(1, hd(Messages)),
  LastNumber  = find_next_gap(Messages),
  if FirstHBQ == LastDQ + 1 ->
      NewMessages = [Message || Message <- Messages, element(1, Message) =< LastNumber],
      DQ  ! {shift, length(NewMessages), tools:get_config_value(server, dlq_limit)},
      DQ  ! {append, NewMessages},
      HBQ ! {replace, [Message || Message <- Messages, element(1, Message) > LastNumber]};
    true -> false
  end
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
  %Bestimmt die letzte Nachrichtennummer in DQ
  LastDQ    = get_last_dq(DQ),
  %Prüft, ob eine Lücke existiert, und füllt sie mit einer Fehlernachricht
  if FirstHBQ > LastDQ + 1 ->
      ErrorMessage  = make_error_message(LastDQ + 1, FirstHBQ - 1),
      DQ  ! {shift, 1, tools:get_config_value(server, dlq_limit)},
      DQ  ! {push, ErrorMessage};
    true -> false
  end
.

get_last_dq(DQ) ->
  tools:synchronized_call(DQ, {getall, self()}, messages, fun(DQMessages) ->
    if DQMessages == [] -> 0;
      true -> element(1, lists:last(DQMessages))
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