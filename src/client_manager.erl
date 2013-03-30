-module(client_manager).
-compile(export_all).

loop(ClientList) ->
  receive
    {set_client_info, {ClientPid, Number, Timestamp}} ->
      loop(lists:keyreplace(ClientPid, 1, ClientList, {ClientPid, Number, Timestamp}));

    {get_client_info, Pid, ClientPid} ->
      Pid ! {client_info, lists:keyfind(ClientPid, 1, ClientList)},
      loop(ClientList)
  end
.