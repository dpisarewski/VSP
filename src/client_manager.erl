-module(client_manager).
-compile([debug_info, export_all]).

loop(ClientList) ->
  receive
    {set_client_info, {ClientPid, Number, Timestamp}} ->
      loop(lists:keystore(ClientPid, 1, ClientList, {ClientPid, Number, Timestamp}));

    {get_client_info, Pid, ClientPid} ->
      Pid ! {client_info, lists:keyfind(ClientPid, 1, ClientList)},
      loop(ClientList)
  end
.