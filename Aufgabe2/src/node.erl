-module(node).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).


start(Name, Edges) ->
  tools:stdout("Node " ++ Name ++ " started"),
  tools:stdout(Edges),

  EdgeStates = dict:new(),
  [dict:store(Edge,sleeping, EdgeStates)|| Edge <-Edges]
.
loop(Name, Level, FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch, EdgeStates)->
  receive
    wakeup ->
      NeuLevel          = 0,
      NeuFoundCounter   = 0,
      NeuNodeState      = found,
      Edge              = lists:min(lists:sort(Edges)),
      dict:store(Edge,branch, EdgeStates),
      getNodePid(Edge, Name) ! {connect,Level,Edge},
      loop(Name, NeuLevel, FragName, NeuNodeState, Edges, BestEdge, WeightBE, NeuFoundCounter, TestEdge, InBranch, EdgeStates);

    {initiate,Level,FragName,NodeState,Edge}  ->
      NeuLevel        = Level,
      NeuFragName     = FragName,
      NeuNodeState    = NodeState,
      NeuInBranch     = Edge,
      NeuBestEdge     = nil,
      NeuWeightBE     = infinity,

      Branches        = [Branch || Branch <- Edges, getEdgeState(Branch, EdgeStates) == branch and Branch =/= Edge],
      [getNodePid(Branch, Name) ! {initiate, NeuLevel, NeuFragName, NeuNodeState, Branch} || Branch <- Branches],
      NeuFoundCounter = FoundCounter + length(Branches),
      if NodeState == find ->
          test();
        true ->
          do_nothing
      end,
      loop(Name, NeuLevel, NeuFragName, NeuNodeState, Edges, NeuBestEdge, NeuWeightBE, NeuFoundCounter, TestEdge, NeuInBranch, EdgeStates);

    {test,Level,FragName,Edge} ->
      if (NodeState == sleeping) ->

        %TODO sich selber aufwäcken
      end,
      loop();

    {accept,Edge}  ->
      NeuTestEdge     = nil,
      if(getWeigt(Edge) < BestEdge) ->
        NeuBestEdge   = Edge,
        NeuWeightBE   = getWeigt(Edge),
      true ->
        NeuBestEdge   = BestEdge,
        NeuWeightBE   = WeightBE,
      end,
      sendReport(Level,FragName, NodeState, Edges, NeuBestEdge, NeuWeightBE, FoundCounter, NeuTestEdge, InBranch, Edge);

    {reject,Edge} ->
      if (getState(Edge) == basic) ->
          setState(Edge, rejected)
      end,
      sendTest(),%TODO
      loop();

    {report,Weight,Edge}  ->
      if Edge =:= InBranch ->
          NeuFoundCounter = FoundCounter - 1,
          case Weight < WeightBE of
            true ->
              NeuBestEdge   = Edge,
              NeuWeight     = Weight;
          end,
          sendReport(Level,FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch, Edge);
        NodeState == find ->
          self() ! {report, Weight, Edge};
        Weight > WeightBE ->
          changeroot();
        Weight == WeightBE == infinity ->
          exit()
      end,
      loop();

    {changeroot,Edge} ->
       if(getState(BestEdge) == branch)->
         getNext(BestEdge)!{changeroot, Edge},
         NeuBestEdge    = BestEdges,
         NeuEdges       = Edges;
       true ->
         getNext(BestEdge)!{connect,Level,Edge},
         NeuBestEdge    = setState(BestEdge, branch);
         NeuEdges       = [NeuBestEdge ,delete(BestEdge)], %neu List von Edges, mit BestEdge als Branch markiert
       end,
      loop(Name, Level,FragName, NodeState, NeuEdges, NeuBestEdge, WeightBE, FoundCounter, TestEdge, InBranch, EdgeStates);

    {connect, ReceivedLevel, Edge} ->
      if (NodeState == sleeping) ->
          self()! wakeup;
        true ->
          do_nothing
      end,
      if ReceivedLevel < Level ->
          dict:store(Edge, branch, EdgeStates),
          getNodePid(Edge, Name) ! {initiate, Level, FragName, NodeState, Edge},
          if NodeState == find ->
              NeuFoundCounter = FoundCounter + 1;
            true ->
              do_nothing
          end,
          getEdgeState(Edge, EdgeStates)

      loop()
  end
.



%liefert zurück die Knote von andere Seite von Edge
getNodePid(Edge, Name) ->
  Source = element(2, Edge),
  if Source == Name ->
      Node = element(3, Edge);
    true ->
      Node = Source
  end,
  global:whereis_name(Node)
.

report(Level, FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch, Edge, EdgeStates) ->
  if (FoundCounter == 0 and TestEdge == nil) ->
      NeuNodeState = found,
      InBranch ! {report, WeightBE, Edge};
    true ->
      NeuNodeState = NodeState
  end
.

changeroot(Level, BestEdge) ->
  stub
.

getEdgeState(Edge, EdgeStates) ->
  dict:fetch(Edge, EdgeStates)
.

test() ->
  stub
.