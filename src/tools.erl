-module(tools).
-compile(export_all).

logging(Datei,Text) ->
  file:write_file(Datei,Text,[append]),
  io:format(Text)
.

stdout(Text) ->
  io:write(io:format(Text))
.

get_config_value(Name) ->
  {ok, Config} = file:consult("server.cfg"),
  return_value_or_false(lists:keyfind(Name, 1, Config))
.

return_value_or_false(Tuple) when Tuple == false -> false;
return_value_or_false(Tuple) ->
  {_, Value} = Tuple,
  Value
.

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
