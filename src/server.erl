-module(server).
-compile(export_all).

start(Name) ->
	HBQ 	      = spawn(fun()-> queue_helper:queue([]) end),
	DQ 		      = spawn(fun()-> queue_helper:queue([]) end),
	Manager     = spawn(fun()-> manager:manager([HBQ, DQ]) end),
	Sender 	    = spawn(fun()-> sender:send_func(DQ) end),
  ClientList  = spawn(fun() -> client_list([]) end),
  Server 	    = spawn(fun()-> loop([HBQ, DQ, Sender, Manager], 1, ClientList) end),
  %register(Name, Server),
  {Server, ClientList}.

loop([HBQ, DQ, Sender, Manager], N, ClientList) ->
	receive
		{getmsgid, Pid} ->
			Pid ! {nnr, N},
			loop([HBQ, DQ, Sender, Manager], N+1, ClientList);
		{getmessages, Pid} ->
			Sender ! {send_messages, Pid},
			loop([HBQ, DQ, Sender, Manager], N, ClientList);
		{dropmessage, {Nachricht, Number}} ->
			HBQ ! {push, {Number, Nachricht}},
			Manager ! validate_queues,
			loop([HBQ, DQ, Sender, Manager], N, ClientList)
	end
.

client_list(ClientList) ->
  receive
    {new_client, ClientPID} ->                                    %% Traegt neue Client in die ClientListe. Nr = 0, weil Client neu ist.
                                                                  %% ClientList erhaelt die Tupeln {ClientPID, Nr}, wo "Nr" die Nummer der letzte empfangene Nachricht ist.
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " war erfolgreich in der Liste hinzugefuegt.~n"),
      client_list(lists:append(ClientList, [{ClientPID, 0}]));

    {delete_client, ClientPID} ->
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " war erfolgreich aus der Liste entfernt.~n"),
      client_list(delete_client_with_pid(ClientList, ClientPID));

    {client_erhalten, ClientPID} ->                               %% Prueft ob Client in der Liste ist.

      client_list(ClientList);

    {getmsgnr, ServerPID, ClientPID} ->                           %% Gibt die Nummer der letzte an den Client gesendete Nachricht.
      Client = find_client(ClientList, ClientPID),
      {_, Nr} = Client,
      ServerPID ! {ClientPID, Nr},
      tools:stdout("Client " ++ werkzeug:to_String(ClientPID) ++ " hat die letzte Nachricht mit der Nummer " ++ werkzeug:to_String(Nr) ++ " empfangen.~n"),
      client_list(ClientList);

    {zeige_client_list} ->
      Text = werkzeug:to_String(ClientList),
      tools:stdout(Text),
      client_list(ClientList)
  end
.

find_client(ClientList, ClientPID) ->
  lists:keyfind(ClientPID, 1, ClientList)
.


delete_client_with_pid(ClientList, ClientPID) ->
  lists:keydelete(ClientPID, 1, ClientList)
.