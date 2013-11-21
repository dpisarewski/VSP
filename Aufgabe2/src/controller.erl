-module(controller).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).

start(Name) ->
  start(Name, "node.cfg")
.

start(Name, Filename) ->
  LogFile     = lists:concat(["log/", "node_", Name, ".log"]),
  %Löschen, falls die Datei schon vorhanden ist
  file:delete(LogFile),

  ping_nodes("hosts"),
  Neighbors = load_neighbors(Filename),
  Edges     = [{element(1, Neighbor), Name, element(2, Neighbor)} || Neighbor <- Neighbors],
  werkzeug:logging(LogFile, "Edges loaded: " ++ werkzeug:to_String(Edges)),
  spawn(fun() ->
    register(Name),
    node:start(LogFile, Name, Edges)
  end)
.

start_all() ->
  {ok, Filenames} = file:list_dir("nodes"),
  [start(extract_node_name(Filename), lists:concat(["nodes/", Filename])) || Filename <- Filenames]
.

extract_node_name(Filename) ->
  re:replace(Filename, "node(\\d)\.cfg", "\\1", [{return, list}])
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