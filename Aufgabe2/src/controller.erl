-module(controller).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).

start(Name, Filename) ->
  ping_nodes("hosts"),
  Neighbors = load_neighbors(Filename),
  Edges = [{element(1, Neighbor), Name, element(2, Neighbor)} || Neighbor <- Neighbors],
  spawn(fun() ->
    register(Name),
    tools:stdout(Edges)
  end)
.

start_all() ->
  {ok, ConfigFiles} = file:list_dir("nodes"),
  [start(re:replace(ConfigFile, "node(\\d)\.cfg", "\\1", [{return, list}]), lists:concat(["nodes/", ConfigFile])) || ConfigFile <- ConfigFiles]
.

ping_nodes(Filename) ->
  {ok, Hostlist} = file:consult(Filename),
  net_adm:world_list(Hostlist)
.

register(Name) ->
  global:register_name(Name, self())
.

load_neighbors(Filename) ->
  read_neighbors(Filename)
.

read_neighbors(Filename) ->
  {ok, ConfigFile} = file:open(Filename, read),
  read_neighbors(ConfigFile, [])
.

read_neighbors(ConfigFile, Neighbors) ->
  case file:read_line(ConfigFile) of
    {ok, Data} ->
      [Weight, Node] = string:tokens(Data, ","),
      Neighbor = {list_to_integer(Weight), string:strip(Node, right, $\n)},
      read_neighbors(ConfigFile, lists:append(Neighbors, [Neighbor]));
    eof ->
      Neighbors
      ;
    {error, _} ->
      []
  end
.