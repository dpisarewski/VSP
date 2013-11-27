-module(controller).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).
-include("data.hrl").

start(Name) ->
  ping_nodes("hosts"),
  start(Name, "node.cfg"),
  global:whereis_name(Name) ! wakeup
.

start_all() ->
  register(controller, self()),
  ping_nodes("hosts"),
  {ok, Filenames} = file:list_dir("nodes"),
  Nodenames       = [extract_node_name(Filename) || Filename <- Filenames],

  [start(Nodename, lists:concat(["nodes/", Nodename, ".cfg"])) || Nodename <- Nodenames],
  global:whereis_name(hd(Nodenames)) ! wakeup,
  receive halt -> halt end,
  [global:whereis_name(Nodename) ! {get_data, self()} || Nodename <- Nodenames],

  Branches = collect_data(dict:new(), length(Nodenames)),
  werkzeug:logging("log/all_nodes.log", werkzeug:to_String(Branches) ++ "\n")
.

start(Name, Filename) ->
  LogFile     = lists:concat(["log/", Name, ".log"]),
  %Löschen, falls die Datei schon vorhanden ist
  file:delete(LogFile),
  file:delete("log/all_nodes.log"),

  Neighbors = load_neighbors(Filename),
  Edges     = [{element(1, Neighbor), Name, element(2, Neighbor)} || Neighbor <- Neighbors],
  werkzeug:logging(LogFile, "Edges loaded: " ++ werkzeug:to_String(Edges) ++ "\n"),

  spawn(fun() ->
    register(Name),
    node:start(LogFile, Name, Edges)
  end)
.

collect_data(Branches, N) when N > 0 ->
  receive
    {data, Data} ->
      collect_data(dict:store(element(1, Data#data.in_branch), Data#data.in_branch, Branches), N - 1)
  end
;
collect_data(Branches, N) when N == 0 ->
  Fun = fun(_, Edge, Edges) -> lists:append(Edges, [Edge]) end,
  dict:fold(Fun, [], Branches)
.

extract_node_name(Filename) ->
  re:replace(Filename, "(\\d)\.cfg", "\\1", [{return, list}])
.

ping_nodes(Filename) ->
  {ok, Hostlist} = file:consult(Filename),
  net_adm:world_list(Hostlist)
.

register(Name) ->
  global:register_name(Name, self())
.

load_neighbors(Filename) ->
  {ok, ConfigFile} = file:open(Filename, read),
  read_neighbors(ConfigFile, [])
.

read_neighbors(ConfigFile, Neighbors) ->
  case file:read_line(ConfigFile) of
    {ok, Data} ->
      case string:tokens(Data, ",") of
        [Weight, Node] ->
          Neighbor = {list_to_integer(Weight), string:strip(Node, right, $\n)},
          read_neighbors(ConfigFile, Neighbors ++ [Neighbor]);
        [] ->
          Neighbors
      end;
    eof ->
      Neighbors
  end
.