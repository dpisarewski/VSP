-module(queue_manager).
-compile([debug_info, export_all]).

push(HBQ, DQ, Message) ->
  %Hängt Information über die Eingangszeit an die Nachricht
  NewMessage = append_hbq_timestamp(Message),
  %Loggt die Nachricht
  tools:log(server, element(2, NewMessage) ++ "|-dropmessage\n"),
  %Prüfen, ob die Nachricht noch nicht abgearbeitet wurde und in HBQ einfügen
  NewHBQ = lists:sort(check_order(HBQ, DQ, NewMessage)),
  %Fragt alle Nachrighten aus HBQ ab
  NewDQ = check_for_gaps(NewHBQ, DQ),
  transfer_messages(NewHBQ, NewDQ)
.

%Fügt die empfangene Nachricht in die HBQ ein, wenn sie kleinere Nummer als die größte Nummer in DQ hat
check_order(HBQ, DQ, Message) ->
  LastDQ    = get_last_dq(DQ),
  if LastDQ < element(1, Message) ->
      %Fügt die neue Nachricht in die HBQ ein
      lists:append(HBQ, [Message]);
    true -> HBQ
  end
.

%Prüft, ob die HBQ voll ist, ob es eine Lücke gibt, und trägt Nachrichten aus HBQ in die DQ über
check_for_gaps(HBQ, DQ) ->
  DQLimit = tools:get_config_value(server, dlq_limit),
  if length(HBQ) >= (DQLimit / 2) ->
      fill_gap(HBQ, DQ);
    true ->
      DQ
  end
.

%Trägt Nachrichten bis zur nächsten Lücke aus HBQ in die DQ
transfer_messages(HBQ, DQ) ->
  if HBQ =/=[]->
    LastDQ      = get_last_dq(DQ),
    FirstHBQ    = element(1, hd(HBQ)),
    LastNumber  = find_next_gap(HBQ),
    if FirstHBQ == LastDQ + 1 ->
        NewMessages = [append_dq_timestamp(Message) || Message <- HBQ, element(1, Message) =< LastNumber],
        DQLimit =  tools:get_config_value(server, dlq_limit),
        NewDQ = lists:append(queue_helper:shift(DQ, length(NewMessages), DQLimit), NewMessages),
        NewHBQ = [Message || Message <- HBQ, element(1, Message) > LastNumber],
        [NewHBQ, NewDQ];
      true -> [HBQ, DQ]
    end;
  true->  [HBQ, DQ]
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
      DQLimit = tools:get_config_value(server, dlq_limit),
      lists:append(queue_helper:shift(DQ, 1, DQLimit), [ErrorMessage]);
    true -> DQ
  end
.

get_last_dq(DQ) ->
  if DQ == [] ->
      0;
    true ->
      element(1, lists:last(DQ))
  end
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