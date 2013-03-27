%% Copyright
-module(client).
-author("denisfleischhauer").

%% API
-export([start/1]).

start(ServerPID) ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, LogDatei} = werkzeug:get_config_value(log_datei, ConfigListe),

  spawn(fun() -> simulation(1, ServerPID, LogDatei) end)
.

getmessageid(ServerPID, LogDatei) ->
  ServerPID ! {getmsgid, self()},
  werkzeug:logging(LogDatei,"-Client: Nachrichtennummer von " ++pid_to_list(ServerPID)++ " angefragt\n")
.

generatemessage(Number) ->
  {ok, Hostname} = inet:gethostname(),
  Praktikumsgruppe = "1",
  Teamnummer = "06",
  Systemsendezeit = werkzeug:timeMilliSecond(),
  lists:concat(["0-client@", Hostname, Praktikumsgruppe, Teamnummer, " : ", Number, "te Nachricht ", Systemsendezeit, "\n"])
.

sendmessage(ServerPID, LogDatei, Nachricht, Number) ->
  ServerPID ! {dropmessage, {Nachricht, Number}},
  werkzeug:logging(LogDatei,"-Client: Nachricht: " ++integer_to_list(Number)++ " an " ++pid_to_list(ServerPID)++ " gesendet\n")
.

simulation(AnzahlSchritte, ServerPID, LogDatei) when AnzahlSchritte < 6 ->
  getmessageid(ServerPID, LogDatei),

  receive
    {nnr, Number} ->
      werkzeug:logging(LogDatei,"-Client: Nachrichtennummer: " ++integer_to_list(Number)++ " von " ++pid_to_list(ServerPID)++ " bekommen\n"),
      Nachricht = generatemessage(Number),
      sendmessage(ServerPID, LogDatei, Nachricht, Number),
      simulation(AnzahlSchritte + 1, ServerPID, LogDatei)
  end

  %{reply,Number,Nachricht,Terminated} ->
.

