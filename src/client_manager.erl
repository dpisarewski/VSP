-module(client_manager).
-compile(export_all).

loop(ClientList) ->
  receive
    {subscribe_client, ClientPID} ->                                    %% Traegt neue Client in die ClientListe. Nr = 0, weil Client neu ist.
                                                                  %% ClientList erhaelt die Tupeln {ClientPID, Nr}, wo "Nr" die Nummer der letzte empfangene Nachricht ist.
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " ist in die Liste eingefuegt.~n"),
      loop(lists:append(ClientList, [{ClientPID, 0}]));

    {unsubscribe_client, ClientPID} ->
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " ist aus der Liste entfernt.~n"),
      loop(delete_client_with_pid(ClientList, ClientPID));

    {increment_message_number, ClientPID} ->
      {_, CurrentNumber} = lists:keyfind(ClientPID, 1, ClientList),
      tools:stdout("Nachrichtennummer fuer Client " ++ werkzeug:to_String(ClientPID) ++ " inkrementiert~n"),
      loop(lists:keyreplace(ClientPID, 1, ClientList, {ClientPID, CurrentNumber + 1}));

    {next_message_number, ServerPID, ClientPID} ->                           %% Gibt die Nummer der letzte an den Client gesendete Nachricht.
      Client = find_client(ClientList, ClientPID),
      {_, Number} = Client,
      ServerPID ! {next_message_number, ClientPID, Number},
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " hat die letzte Nachricht mit der Nummer " ++ werkzeug:to_String(Number) ++ " empfangen.~n"),
      loop(ClientList);

    {get_client_list, Pid} ->
      Pid ! {client_list, ClientList},
      loop(ClientList)
  end
.

find_client(ClientList, ClientPID) ->
  lists:keyfind(ClientPID, 1, ClientList)
.


delete_client_with_pid(ClientList, ClientPID) ->
  lists:keydelete(ClientPID, 1, ClientList)
.