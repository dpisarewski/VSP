-module(sender).
-compile([debug_info, export_all]).

send_func(DQ, ClientManager) ->
	receive
		{send_messages, Pid} ->
			tools:synchronized_call(DQ, {getall, self()}, messages, fun(Messages)->
        NextMessageNumber = update_client_info(ClientManager, Pid, Messages),
        send_message(Pid, Messages, NextMessageNumber)
      end),
			send_func(DQ, ClientManager)
	end
.

send_message(Pid, Messages, Number) ->
  %tools:stdout("sending message " ++ werkzeug:to_String(Number) ++ " to client " ++ pid_to_list(Pid) ++ "~n"),
  MessagesAfter     = [Message || Message <- Messages, element(1, Message) > Number],
  [{Number, Text}]  = [Message || Message <- Messages, element(1, Message) == Number],
  werkzeug:logging("server.log", Text ++ "|.(" ++ werkzeug:to_String(Number) ++ ")-getmessages von " ++ werkzeug:to_String(Pid) ++ "-" ++ werkzeug:to_String(MessagesAfter == []) ++ "\n"),
	Pid ! {reply, Number, Text, MessagesAfter == []}
.

update_client_info(ClientManager, ClientPid, Messages) ->
  tools:synchronized_call(ClientManager, {get_client_info, self(), ClientPid}, client_info, fun(Response) ->
    NewNumber = compute_new_number(extract_info(Response, ClientPid), Messages),
    ClientManager ! {set_client_info, {ClientPid, NewNumber, now()}},
    NewNumber
  end)
.

init_client(ClientPid) ->
  {ClientPid, 0, now()}
.

first_message_number(Messages) ->
  element(1, hd(Messages))
.

extract_info(Response, ClientPid) ->
  if
    Response =/= false ->
      Response;
    true ->
      init_client(ClientPid)
  end
.

compute_new_number({ClientPid, Number, Timestamp}, Messages) ->
  Expired = timer:now_diff(now(), Timestamp) / 1000 > timer:seconds(tools:get_config_value(clientlifetime)),
  if
    Expired ->
      werkzeug:logging("server.log", "Client " ++ werkzeug:to_String(ClientPid) ++ " wird vergessen! *************\n"),
      first_message_number(Messages);
    true ->
      next_message_number(Messages, Number)
  end
.

next_message_number(Messages, Number) ->
  MessagesAfter = [Message || Message <- Messages, element(1, Message) > Number],
  if
    MessagesAfter == [] ->
      Number;
    true ->
      first_message_number(MessagesAfter)
  end
.