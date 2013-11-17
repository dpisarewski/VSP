-module(node).
-author("Dieter Pisarewski, Maxim Rjabenko").
-compile(export_all).


start(Name, Edges) ->
  tools:stdout("Node " ++ Name ++ " started"),
  tools:stdout(Edges)
.

