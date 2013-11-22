-module(node).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).
-include("data.hrl").

start(LogFile, Name, Edges) ->
  tools:stdout("Node " ++ Name ++ " started"),
  EdgeStates  = initEdgeStates(Edges),
  Data = #data{
    name        = Name,
    edges       = Edges,
    edge_states = EdgeStates,
    log_file    = LogFile
  },
  loop(Data)
.

loop(Data) ->
  receive
    wakeup ->
      log(Data, lists:concat([Data#data.name, ": wakeup"])),
      loop(wakeup(Data));

    {initiate, ReceivedLevel, ReceivedFragName, ReceivedNodeState, ReceivedEdge}  ->
      Edge        = convert_edge(ReceivedEdge, Data),
      NewData     = init_fragment(Data, ReceivedLevel, ReceivedFragName, ReceivedNodeState, Edge),
      ChangedData = broadcast_initiate(NewData, Edge),
      if ChangedData#data.node_state == find ->
          UpdatedData = test(ChangedData);
        true ->
          UpdatedData = ChangedData
      end,
      loop(UpdatedData);

    {test, ReceivedLevel, ReceivedFragName, ReceivedEdge} ->
      NewData = check_state(Data),
      Edge    = convert_edge(ReceivedEdge, Data),
      loop(accept_or_reject_edge(NewData, ReceivedLevel, ReceivedFragName, Edge));

    {accept, ReceivedEdge}  ->
      Edge    = convert_edge(ReceivedEdge, Data),
      Weight  = getWeigt(Edge),
      if Weight < Data#data.best_edge ->
        BestEdge    = Edge,
        WeightBE    = getWeigt(Edge);
      true ->
        BestEdge    = Data#data.best_edge,
        WeightBE    = Data#data.best_edge
      end,
      NewData       = Data#data{
        test_edge   = nil,
        best_edge   = BestEdge,
        best_weight = WeightBE
      },
      loop(report(NewData));

    {reject, ReceivedEdge} ->
      Edge      = convert_edge(ReceivedEdge, Data),
      EdgeState = getEdgeState(Edge, Data),
      if EdgeState == basic ->
          NewData = Data#data{edge_states = setEdgeState(Edge, Data, rejected)};
        true ->
          NewData = Data
      end,
      loop(test(NewData));

    {report, Weight, ReceivedEdge}  ->
      Edge = convert_edge(ReceivedEdge, Data),
      if Edge /= Data#data.in_branch ->
          NewData = convergecast_report(Data, Weight, Edge);
        Data#data.node_state == find ->
          self() ! {report, Weight, Edge},
          NewData = Data;
        Weight > Data#data.best_weight ->
          NewData = changeroot(Data);
        (Weight == Data#data.best_weight) and (Weight == infinity) ->
          NewData = Data,
          tools:stdout("Node " ++ Data#data.name ++ " stopped\n"),
          whereis(controller) ! halt,
          loop(NewData);
        true ->
          NewData = Data
      end,
      loop(NewData);

    {changeroot, _} ->
      loop(changeroot(Data));

    {connect, ReceivedLevel, ReceivedEdge} ->
      NewData   = check_state(Data),
      Edge      = convert_edge(ReceivedEdge, Data),
      EdgeState = getEdgeState(Edge, NewData),
      if ReceivedLevel < NewData#data.level ->
          ChangedData = absorb_fragment(NewData, Edge);
        EdgeState == basic ->
          self() ! {connect, ReceivedLevel, Edge},
          ChangedData = NewData;
        true ->
          getNodePid(Edge, NewData) ! {initiate, NewData#data.level + 1, werkzeug:to_String(getWeigt(Edge)), find, Edge},
          log(Data, lists:concat([NewData#data.name, "->", neighbor(Edge, NewData), ": {initiate, ", NewData#data.level + 1, ", ", werkzeug:to_String(getWeigt(Edge)), ", ", find, ", ", werkzeug:to_String(Edge), "}"])),
          ChangedData = NewData
      end,
      loop(ChangedData);

    {get_data, Pid} ->
      Pid ! {data, Data},
      loop(Data)
  end
.

check_state(Data) ->
  if Data#data.node_state == sleeping ->
      wakeup(Data);
    true ->
      Data
  end
.

wakeup(Data) ->
  Edge          = lists:min(Data#data.edges),
  NewData       = Data#data{
    found_count = 0,
    level       = 0,
    node_state  = found,
    edge_states = setEdgeState(Edge, Data, branch)
  },
  getNodePid(Edge, NewData) ! {connect, NewData#data.level, Edge},
  log(NewData, lists:concat([NewData#data.name, "->", neighbor(Edge, NewData), ": {connect, ", NewData#data.level, ", ", werkzeug:to_String(Edge), "}"])),
  NewData
.

test(Data) ->
  BasicEdges = [Edge || Edge <- Data#data.edges, getEdgeState(Edge, Data) == basic],
  if length(BasicEdges) /= 0 ->
      NewData     = Data#data{test_edge = lists:min(BasicEdges)},
      getNodePid(NewData#data.test_edge, NewData) ! {test, NewData#data.level, NewData#data.frag_name, NewData#data.test_edge},
      log(NewData, lists:concat([NewData#data.name, "->", neighbor(NewData#data.test_edge, NewData), ": {test, ", NewData#data.level, ", ", NewData#data.frag_name, ", ", werkzeug:to_String(NewData#data.test_edge), "}"]));
    true ->
      NewData     =  report(Data#data{test_edge = nil})
     end,
  NewData
.


report(Data) ->
  if (Data#data.found_count == 0) and (Data#data.test_edge == nil) ->
      NewData = Data#data{node_state = found},
      getNodePid(NewData#data.in_branch, NewData) ! {report, NewData#data.best_weight, NewData#data.in_branch},
      log(NewData, lists:concat([NewData#data.name, "->", neighbor(NewData#data.in_branch, NewData), ": {report, ", NewData#data.best_weight, ", ", werkzeug:to_String(NewData#data.in_branch), "}"]));
    true ->
      NewData = Data
  end,
  NewData
.

changeroot(Data) ->
  EdgeState = getEdgeState(Data#data.best_edge, Data),
  if EdgeState == branch ->
      getNodePid(Data#data.best_edge, Data) ! {changeroot, Data#data.best_edge},
      log(Data, lists:concat([Data#data.name, "->", neighbor(Data#data.best_edge, Data), ": {changeroot, ", werkzeug:to_String(Data#data.best_edge), "}"])),
      NewData = Data;
    true ->
      getNodePid(Data#data.best_edge, Data) ! {connect, Data#data.level, Data#data.best_edge},
      log(Data, lists:concat([Data#data.name, "->", neighbor(Data#data.best_edge, Data), ": {connect, ", Data#data.level, ", ", werkzeug:to_String(Data#data.best_edge), "}"])),
      NewData = Data#data{edge_states = setEdgeState(Data#data.best_edge, Data, branch)}
  end,
  NewData
.

%liefert zurück die Knote von andere Seite von Edge
getNodePid(Edge, Data) ->
  global:whereis_name(neighbor(Edge, Data))
.

neighbor(Edge, Data) ->
  Source = element(2, Edge),
  if Source == Data#data.name ->
      element(3, Edge);
    true ->
      Source
  end
.

getEdgeState(Edge, Data) ->
  dict:fetch(Edge, Data#data.edge_states)
.

setEdgeState(Edge, Data, NewState) ->
  dict:store(Edge, NewState, Data#data.edge_states)
.

getWeigt(Edge) ->
  element(1, Edge)
.

initEdgeStates(Edges) ->
  Fun = fun(Edge, Dictionary) -> dict:store(Edge, basic, Dictionary) end,
  lists:foldl(Fun, dict:new(), Edges)
.


rejectEdge(Edge, Data) ->
  EdgeState = getEdgeState(Edge, Data),
  if EdgeState == basic ->
      NewData = Data#data{edge_states = setEdgeState(Edge, Data, rejected)};
    true ->
      NewData = Data
  end,
  if NewData#data.test_edge /= Edge ->
      getNodePid(Edge, NewData) ! {reject, Edge},
      log(NewData, lists:concat([NewData#data.name, "->", neighbor(Edge, NewData), ": {reject, ", werkzeug:to_String(Edge), "}"])),
      ChangedData = NewData;
    true ->
      ChangedData = test(NewData)
  end,
  ChangedData
.

accept_or_reject_edge(Data, ReceivedLevel, ReceivedFragName, Edge) ->
  if Data#data.level < ReceivedLevel ->
      self() ! {test, ReceivedLevel, ReceivedFragName, Edge},
      NewData = Data;
    Data#data.frag_name /= ReceivedFragName ->
      getNodePid(Edge, Data) ! {accept, Edge},
      log(Data, lists:concat([Data#data.name, "->", neighbor(Edge, Data), ": {accept, ", werkzeug:to_String(Edge), "}"])),
      NewData = Data;
    true ->
      NewData = rejectEdge(Edge, Data)
  end,
  NewData
.

broadcast_initiate(Data, Edge) ->
  Branches        = [Branch || Branch <- Data#data.edges, (getEdgeState(Branch, Data) == branch) and (Branch /= Edge)],
  [send_initiate(Branch, Data) || Branch <- Branches],
  Data#data{found_count = Data#data.found_count + length(Branches)}
.

send_initiate(Edge, Data) ->
  getNodePid(Edge, Data) ! {initiate, Data#data.level, Data#data.frag_name, Data#data.node_state, Edge},
  log(Data, lists:concat([Data#data.name, "->", neighbor(Edge, Data), ": {initiate, ", Data#data.level, ", ", Data#data.frag_name, ", ", Data#data.node_state, ", ", werkzeug:to_String(Edge), "}"]))
.

init_fragment(Data, ReceivedLevel, ReceivedFragName, ReceivedNodeState, Edge) ->
  Data#data{
    level           = ReceivedLevel,
    frag_name       = ReceivedFragName,
    node_state      = ReceivedNodeState,
    in_branch       = Edge,
    best_edge       = nil,
    best_weight     = infinity
  }
.

convergecast_report(Data, Weight, Edge) ->
  if Weight < Data#data.best_weight ->
      NewBestEdge   = Edge,
      NewWeight     = Weight;
    true ->
      NewBestEdge   = Data#data.best_edge,
      NewWeight     = Data#data.best_weight
  end,
  NewData = Data#data{
    found_count = Data#data.found_count - 1,
    best_edge   = NewBestEdge,
    best_weight = NewWeight
  },
  report(NewData)
.

absorb_fragment(Data, Edge) ->
  NewEdgeStates = setEdgeState(Edge, Data, branch),
  send_initiate(Edge, Data),
  if Data#data.node_state == find ->
      NewFoundCounter = Data#data.found_count + 1;
    true ->
      NewFoundCounter = Data#data.found_count
  end,
  Data#data{
    edge_states = NewEdgeStates,
    found_count = NewFoundCounter
  }
.

convert_edge(Edge, Data) ->
  {element(1, Edge), Data#data.name, neighbor(Edge, Data)}
.

log(Data, Message) ->
  werkzeug:logging("log/all_nodes.log", Message ++ "\n"),
  werkzeug:logging(Data#data.log_file, Message ++ "\n")
.
