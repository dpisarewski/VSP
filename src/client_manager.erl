-module(client_manager).
-compile(export_all).

loop(ClientList) ->
  receive
    {new_client, ClientPID} ->                                    %% Traegt neue Client in die ClientListe. Nr = 0, weil Client neu ist.
                                                                  %% ClientList erhaelt die Tupeln {ClientPID, Nr}, wo "Nr" die Nummer der letzte empfangene Nachricht ist.
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " war erfolgreich in der Liste hinzugefuegt.~n"),
      loop(lists:append(ClientList, [{ClientPID, 0}]));

    {delete_client, ClientPID} ->
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " war erfolgreich aus der Liste entfernt.~n"),
      loop(delete_client_with_pid(ClientList, ClientPID));

    {client_erhalten, ClientPID} ->                               %% Prueft ob Client in der Liste ist.

      loop(ClientList);

    {getmsgnr, ServerPID, ClientPID} ->                           %% Gibt die Nummer der letzte an den Client gesendete Nachricht.
      Client = find_client(ClientList, ClientPID),
      {_, Nr} = Client,
      ServerPID ! {ClientPID, Nr},
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " hat die letzte Nachricht mit der Nummer " ++ werkzeug:to_String(Nr) ++ " empfangen.~n"),
      loop(ClientList);

    {zeige_client_list} ->
      Text = werkzeug:to_String(ClientList),
      tools:stdout(Text),
      loop(ClientList)
  end
.

find_client(ClientList, ClientPID) ->
  lists:keyfind(ClientPID, 1, ClientList)
.


delete_client_with_pid(ClientList, ClientPID) ->
  lists:keydelete(ClientPID, 1, ClientList)
.