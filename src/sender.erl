-module(sender).
-compile(export_all).

send_func(DQ, ClientManager) ->
	receive
		{send_messages, Pid} ->
			tools:synchronized_call(DQ, {getall, self()}, messages, fun(Messages)->
        tools:stdout("Received Messages~n"),
        NextMessageNumber = update_client_info(ClientManager, Pid, Messages),
        send_message(Pid, Messages, NextMessageNumber)
      end),
			send_func(DQ, ClientManager)
	end
.

send_message(Pid, Messages, Number) ->
  tools:stdout("sending message " ++ werkzeug:to_String(Number) ++ " to client " ++ pid_to_list(Pid) ++ "~n"),
  MessagesAfter     = [Message || Message <- Messages, element(1, Message) > Number],
  [{Number, Text}]  = [Message || Message <- Messages, element(1, Message) == Number],
	Pid ! {reply, Number, Text, MessagesAfter == []}
.

update_client_info(ClientManager, Pid, Messages) ->
  tools:synchronized_call(ClientManager, {get_client_info, self(), Pid}, client_info, fun(Response) ->
    %FIXME too ugly function
    tools:stdout(lists:concat(["Received client info ", werkzeug:to_String(Response), "~n"])),
    {_, Number, Timestamp} = if
      Response =/= false ->
        Response;
      true ->
        init_client(Pid)
    end,
    Expired = timer:now_diff(now(), Timestamp) / 1000 > timer:seconds(tools:get_config_value(clientlifetime)),
    NewNumber = if
      Expired ->
        first_message_number(Messages);
      true ->
        MessagesAfter = [Message || Message <- Messages, element(1, Message) > Number],
        if
          MessagesAfter == [] ->
            Number;
          true ->
            first_message_number(MessagesAfter)
        end
    end,
    ClientManager ! {set_client_info, {Pid, NewNumber, now()}},
    NewNumber
  end)
.

init_client(ClientPid) ->
  {ClientPid, 0, now()}
.

first_message_number(Messages) ->
  element(1, hd(Messages))
.