-module(node).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).


start(Name, Edges) ->
  tools:stdout("Node " ++ Name ++ " started"),
  tools:stdout(Edges)
.

loop(Level,FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch)->
  receive
    {wakeup} ->
      Edge            = min(sort(Edges)),
      NeuNodeState    = found,
      getNext(Edge) ! {connect,Level,Edge},
      loop(Level,FragName, NeuNodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch);

    {initiate,Level,FragName,NodeState,Edge}  ->
      NeuLevel        = Level,
      NeuFragName     = FragName,
      NeuNodeState    = NodeState,
      NeuInBranch     = Edge,
      NeuBestEdge     = nil,
      NeuWeightBE     = infinity,
      IstBranch       = fun(X) -> getState(X) == basic,
      BasicEdges      = lists:filter(IstBranch,Edges),
      [getNext(BasicEdge)!{initiate, NeuLevel, NeuFragName, NeuNodeState, Edge} || BasicEdge <- BasicEdges],
      NeuFoundCounter = length(BasicEdges) + FoundCounter,
      if (NodeState == find) ->
        sendTest()   %TODO
      end,
      loop(NeuLevel, NeuFragName, NeuNodeState, Edges, NeuBestEdge, NeuWeightBE, NeuFoundCounter, TestEdge, NeuInBranch);

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
        sendReport(Level,FragName, NodeState, Edges, NeuBestEdge, NeuWeightBE, FoundCounter, NeuTestEdge, InBranch, Edge);
      true ->
        sendReport(Level,FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, NeuTestEdge, InBranch, Edge)
      end;

    {reject,Edge} ->
      if (getState(Edge) == basic) ->
          setState(Edge, rejected)
      end,
      sendTest(),%TODO
      loop();

    {report,Weight,Edge}  ->
      if (Edge =:= InBranch) ->
        NeuFoundCounter = FoundCounter - 1,
        if (Weight < WeightBE) ->
          NeuBestEdge   = Edge,
          NeuWeight     = Weight
        end,
        sendReport(Level,FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch, Edge);
      try ->
        if(NodeState == find)->
          %TODO place received message on end of queue
          ;
        try ->
          if(Weight > WeightBE) ->
            %TODO execute procedure change-root
            ;
          try ->
            if(Weight == WeightBE) ->
              %TODO halt
            end
          end
        end
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
      loop(Level,FragName, NodeState, NeuEdges, NeuBestEdge, WeightBE, FoundCounter, TestEdge, InBranch);

    {connect,Level,Edge} ->
      if (NodeState == sleeping) ->
      %TODO sich selber aufwäcken

      loop()
  end
.



%liefert zurück die Knote von andere Seite von Edge
getNext(Edge) ->
 %TODO
.

sendReport(Level,FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch, Edge)->
  if (FoundCounter == 0 and TestEdge == nil) ->
    NeuNodeState = found;
    InBranch ! {report, WeightBE, Edge},
    loop(Level,FragName, NeuNodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch)
  end,
  loop(Level,FragName, NodeState, Edges, BestEdge, WeightBE, FoundCounter, TestEdge, InBranch)
.