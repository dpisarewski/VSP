-module(server).
-compile(export_all).

start(Name) ->
	HBQ 	  = spawn(fun()-> queue_helper:queue(dict:new()) end),
	DQ 		  = spawn(fun()-> queue_helper:queue(dict:new()) end),
	Manager = spawn(fun()-> manager:manager([HBQ, DQ]) end),
	Sender 	= spawn(fun()-> sender:send_func(DQ) end),
	Server 	= spawn(fun()-> loop([HBQ, DQ, Sender, Manager], 1) end),
	%register(Name, Server),
	Server.

loop([HBQ, DQ, Sender, Manager], N) ->
	receive
		{getmsgid, Pid} ->
			Pid ! {nnr, N},
			loop([HBQ, DQ, Sender, Manager], N+1);
		{getmessages, Pid} ->
			Sender ! {send_messages, Pid},
			loop([HBQ, DQ, Sender, Manager], N);
		{dropmessage, {Nachricht, Number}} ->
			HBQ ! {push, {Number, Nachricht}},
			Manager ! validate_queues,
			loop([HBQ, DQ, Sender, Manager], N)
	end
.
