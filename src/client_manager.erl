-module(client_manager).
-compile([debug_info, export_all]).

loop(ClientList) ->
  receive
    %Speichert letzte gesendete Nachrichtennummer und letzte Zeit der Kommunikation mit einem Client
    {set_client_info, {ClientPid, Number, Timestamp}} ->
      loop(lists:keystore(ClientPid, 1, ClientList, {ClientPid, Number, Timestamp}));

    %Gibt letzte gesendete Nachrichtennummer und letzte Zeit der Kommunikation mit einem Client zurück
    {get_client_info, Pid, ClientPid} ->
      Pid ! {client_info, lists:keyfind(ClientPid, 1, ClientList)},
      loop(ClientList)
  end
.