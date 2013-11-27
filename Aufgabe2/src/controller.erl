-module(controller).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).
-include("data.hrl").

% Einen Knoten starten
start(Name) ->
  %mit allen Rechnern verbinden
  ping_nodes("hosts"),
  %Knoten starten
  start(Name, "node.cfg"),
  %Den Knoten wecken
  global:whereis_name(Name) ! wakeup
.

% mehrere Nodes starten
start_all() ->
  %Den Kontrollerprozess
  register(controller, self()),
  %mit allen Rechnern verbinden
  ping_nodes("hosts"),
  %Knotennamen laden
  {ok, Filenames} = file:list_dir("nodes"),
  Nodenames       = [extract_node_name(Filename) || Filename <- Filenames],

  %Knotenprozesse starten
  [start(Nodename, lists:concat(["nodes/", Nodename, ".cfg"])) || Nodename <- Nodenames],
  %Einen Knoten wecken
  global:whereis_name(hd(Nodenames)) ! wakeup,

  %Auf das Ende der Berechnung warten
  receive halt -> halt end,
  %Daten von Knoten abfragen
  [global:whereis_name(Nodename) ! {get_data, self()} || Nodename <- Nodenames],

  %Daten von Knoten empfangen und Kanten des Spannbaums auswählen
  Branches = collect_data(dict:new(), length(Nodenames)),
  werkzeug:logging("log/all_nodes.log", werkzeug:to_String(Branches) ++ "\n")
.

start(Name, Filename) ->
  %Logdateinamen generieren
  LogFile     = lists:concat(["log/", Name, ".log"]),
  %Löschen, falls Logdateien schon vorhanden sind
  file:delete(LogFile),
  file:delete("log/all_nodes.log"),

  %Nachbarknoten aus der Konfigurationsdatei laden
  Neighbors = load_neighbors(Filename),
  %Kanten generieren
  Edges     = [{element(1, Neighbor), Name, element(2, Neighbor)} || Neighbor <- Neighbors],
  werkzeug:logging(LogFile, "Edges loaded: " ++ werkzeug:to_String(Edges) ++ "\n"),

  %Knotenprozess starten
  spawn(fun() ->
    register(Name),
    node:start(LogFile, Name, Edges)
  end)
.

%Empfängt Daten von Knoten und speichert Branches des Spannbaums
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

%Extrahiert Knotennamen aus einem Dateinamen
extract_node_name(Filename) ->
  re:replace(Filename, "(\\d)\.cfg", "\\1", [{return, list}])
.

%Sendet ping an alle angegebene Rechner
ping_nodes(Filename) ->
  {ok, Hostlist} = file:consult(Filename),
  net_adm:world_list(Hostlist)
.

%Registriert den eigenen Prozess mit dem angegebenen Namen
register(Name) ->
  global:register_name(Name, self())
.

%Lädt Nachbarknoten aus einer Konfigrationsdatei(Gewicht und Name)
load_neighbors(Filename) ->
  {ok, ConfigFile} = file:open(Filename, read),
  read_neighbors(ConfigFile, [])
.

read_neighbors(ConfigFile, Neighbors) ->
  %Liest eine Zeile aus der Konfigurationsdatei
  case file:read_line(ConfigFile) of
    {ok, Data} ->
      %Gewicht und Knotennamen aus der Zeile extrahieren
      case string:tokens(Data, ",") of
        [Weight, Node] ->
          Neighbor = {list_to_integer(Weight), string:strip(Node, right, $\n)},
          %Fügt den neuen NachbarKnoten der Liste hinzu
          read_neighbors(ConfigFile, Neighbors ++ [Neighbor]);
        %Gibt Liste von Nachbarn zurück, wenn die Zeile leer ist
        [] ->
          Neighbors
      end;
    %Gibt Liste von Nachbarn zurück, wenn das Ende der Datei erreicht wurde
    eof ->
      Neighbors
  end
.