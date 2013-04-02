-module(client_manager).
-compile(export_all).

loop(ClientList) ->
  receive
    {set_client_info, {ClientPid, Number, Timestamp}} ->
      tools:stdout("Setting client info " ++ werkzeug:to_String(lists:keystore(ClientPid, 1, ClientList, {ClientPid, Number, Timestamp})) ++ "~n"),
      loop(lists:keystore(ClientPid, 1, ClientList, {ClientPid, Number, Timestamp}));

    {get_client_info, Pid, ClientPid} ->
      tools:stdout("Looking up client info from " ++ werkzeug:to_String(ClientList) ++ "~n"),
      Pid ! {client_info, lists:keyfind(ClientPid, 1, ClientList)},
      loop(ClientList)
  end
.