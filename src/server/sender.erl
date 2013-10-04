-module(sender).
-compile([debug_info, export_all]).

send_func(DQ, ClientManager) ->
	receive
    %Sendet neue Nachricht an den Client
		{send_messages, Pid} ->
      %Holt alle Nachrichten aus DQ, um die letzte gesendete Nachrichtennummer zu bestimmen
			tools:synchronized_call(DQ, {getall, self()}, messages, fun(Messages)->
        %Aktualisiert die letzte gesendete Nachrichtennummer und die Zeit der Kommunikation mit dem Client
        NextMessageNumber = update_client_info(ClientManager, Pid, Messages),
        %Sendet eine neue Nachricht an den Client
        send_message(Pid, Messages, NextMessageNumber)
      end),
			send_func(DQ, ClientManager)
	end
.

%Sendet eine Nachricht mit angegebener Nachrichtennummer an den Client
send_message(Pid, [], _) ->
  Text = "Dummy message. ", Number = 0,
  tools:log(server, Text ++ "|.(" ++ werkzeug:to_String(Number) ++ ")-getmessages von " ++ werkzeug:to_String(Pid) ++ "-" ++ werkzeug:to_String(true) ++ "\n"),
  Pid ! {reply, Number, Text, true}
;
send_message(Pid, Messages, Number) ->
  MessagesAfter     = [Message || Message <- Messages, element(1, Message) > Number],
  [{Number, Text}]  = [Message || Message <- Messages, element(1, Message) == Number],
  tools:log(server, Text ++ "|.(" ++ werkzeug:to_String(Number) ++ ")-getmessages von " ++ werkzeug:to_String(Pid) ++ "-" ++ werkzeug:to_String(MessagesAfter == []) ++ "\n"),
	Pid ! {reply, Number, Text, MessagesAfter == []}
.

%Aktualisiert Clientinformation(letzte gesendete Nachrichtennummer und die Zeit der Kommunikation)
update_client_info(ClientManager, ClientPid, Messages) ->
  %Fragt Clientinformation beim Client Manager
  tools:synchronized_call(ClientManager, {get_client_info, self(), ClientPid}, client_info, fun(Response) ->
    %Kalkuliert neue Nachrichtennummer
    NewNumber = compute_new_number(extract_info(Response, ClientPid, Messages), Messages),
    %Speichert Clientinformationen in Clientmanager
    ClientManager ! {set_client_info, {ClientPid, NewNumber, now()}},
    NewNumber
  end)
.

%Initialisiert Clientinformation
init_client(ClientPid, _) ->
  {ClientPid, 0, now()}
.

%Gibt die erste Nachrichtennummer aus der Liste der Nachrichten zurück
first_message_number([]) ->
  0
;
first_message_number(Messages) ->
  element(1, hd(Messages))
.

%Extrahiert Clientinformation aus der Antwort vom Clientmanager oder generiert neue
extract_info(Response, ClientPid, Messages) ->
  if
    Response =/= false ->
      Response;
    true ->
      init_client(ClientPid, Messages)
  end
.

%Prüft, ob die vergangene Zeit seit der letzten Kommunikation einen vorgegebenen Wert überschreitet, und setzt in diesem Fall die Nachrichtennummer zurück
compute_new_number({ClientPid, Number, Timestamp}, Messages) ->
  Expired = timer:now_diff(now(), Timestamp) / 1000 > timer:seconds(tools:get_config_value(server, clientlifetime)),
  if
    Expired ->
      tools:log(server, "Client " ++ werkzeug:to_String(ClientPid) ++ " wird vergessen! *************\n"),
      first_message_number(Messages);
    true ->
      next_message_number(Messages, Number)
  end
.

%Bestimmt neue Nachrichtennummer, die an den Client zu senden ist
next_message_number(Messages, Number) ->
  MessagesAfter = [Message || Message <- Messages, element(1, Message) > Number],
  if
    MessagesAfter == [] ->
      Number;
    true ->
      first_message_number(MessagesAfter)
  end
.