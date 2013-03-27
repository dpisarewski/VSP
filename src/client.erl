%% Copyright
-module(client).
-author("denisfleischhauer").

%% API
-export([start/1]).

start(ServerPID) ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, LogDatei} = werkzeug:get_config_value(log_datei, ConfigListe),

  spawn(fun() -> simulation(5, ServerPID, LogDatei) end)
.

simulation(AnzahlSchritte, ServerPID, LogDatei) ->
  ServerPID ! {getmsgid, self()},
  werkzeug:logging(LogDatei,"-Client: Nachrichtennummer von " ++pid_to_list(ServerPID)++ " angefragt\n"),
  receive
    {nnr, Number} ->
      werkzeug:logging(LogDatei,"-Client: Nachrichtennummer: " ++integer_to_list(Number)++ " von " ++pid_to_list(ServerPID)++ " bekommen\n"),

      {ok, Hostname} = inet:gethostname(),
      Praktikumsgruppe = "1",
      Teamnummer = "06",
      Systemsendezeit = werkzeug:timeMilliSecond(),
      Nachricht = lists:concat(["0-client@", Hostname, Praktikumsgruppe, Teamnummer, " : ", Number, "te Nachricht ", Systemsendezeit, "\n"]),

      ServerPID ! {dropmessage, {Nachricht, Number}},
      werkzeug:logging(LogDatei,"-Client: Nachricht: " ++integer_to_list(Number)++ " an " ++pid_to_list(ServerPID)++ " gesendet\n")
  end

  %{reply,Number,Nachricht,Terminated} ->
.

