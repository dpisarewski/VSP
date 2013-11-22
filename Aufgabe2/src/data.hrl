-record(data, {
  name        = "",
  level       = 0,
  frag_name   = nil,
  node_state  = sleeping,
  edges       = [],
  best_edge   = nil,
  best_weight = infinity,
  found_count = 0,
  test_edge   = nil,
  in_branch   = nil,
  edge_states = [],
  log_file    = ""
}).