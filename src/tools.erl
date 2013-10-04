-module(tools).
-compile([debug_info, export_all]).

stdout(Text) ->
  io:fwrite(Text)
.

get_config_value(ConfigFile, Name) ->
  Result = file:consult("config/" ++ ConfigFile ++ ".cfg"),
  if element(1, Result) == ok ->
      {ok, Config} = Result,
      return_value_or_false(lists:keyfind(Name, 1, Config));
    true ->
      throw(lists:concat(["could not find configuration parameter: ", Name]))
  end
.

return_value_or_false(Tuple) when Tuple == false -> false;
return_value_or_false(Tuple) ->
  {_, Value} = Tuple,
  Value
.

%Prüft, ob ein Prozess mit angegebenem Namen registriert ist, töten ihn und registriert neuen Prozess mit diesem Namen
reregister(Name, Pid) ->
  OldPid = whereis(Name),
  if
    OldPid =/= undefined ->
      exit(OldPid, kill),
      stdout("Killed process " ++ werkzeug:to_String(Name) ++ " Pid: " ++ werkzeug:to_String(OldPid) ++ "~n");
    true ->
      false
  end,
  register(Name, Pid)
.

%Sendet einem Process eine Nachricht mit Params und wartet auf eine Antwort mit dem Label ResponseLabel von diesem Process. Anchschließend führt den Callcack aus.
synchronized_call(Process, Params, ResponseLabel, Callback) ->
  Process ! Params,
  receive
    {ResponseLabel, Data}->
      Callback(Data)
  end
.

times(0, Fun) ->
  do_nothing
;
times(N, Fun) ->
  Fun(),
  times(N - 1, Fun)
.

log(ConfigFile, Message) ->
  File = get_config_value(ConfigFile, log_datei),
  werkzeug:logging(File, Message)
.