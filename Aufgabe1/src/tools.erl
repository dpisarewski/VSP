-module(tools).
-compile([debug_info, export_all]).

stdout(Text) ->
  io:fwrite(Text)
.

get_config_value(ConfigFile, Name) ->
  Result = file:consult(lists:concat(["config/", ConfigFile,".cfg"])),
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
      stdout(lists:concat(["Killed process ", werkzeug:to_String(Name), " Pid: ", werkzeug:to_String(OldPid), "~n"]));
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

times(N, Fun) ->
  upto(1, N, Fun)
.

upto(From, To, Fun) when From =< To ->
  Fun(From),
  upto(From + 1, To, Fun);
upto(_, _, _) ->
  do_nothing.

log(ConfigFile, Message) ->
  File = get_config_value(ConfigFile, log_file),
  werkzeug:logging(File, Message)
.