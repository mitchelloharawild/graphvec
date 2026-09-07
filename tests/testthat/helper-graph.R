# Equivalent node_vecs/edge_vecs hold different GraphBackend pointers, so
# expect_equal() never finds them equal. Compare what they hold instead: the
# node values, the edges' endpoints and attributes, and `directed`.
graph_contents <- function(x) {
  if (inherits(x, "edge_vec")) {
    nodes <- attr(x, "nodes")
    edges <- as.data.frame(x)
  } else {
    nodes <- node_vec_data(x)
    edges <- as.data.frame(edges(x))
  }
  list(class = class(x), nodes = nodes, edges = edges, directed = attr(x, "directed"))
}

expect_same_graph <- function(object, expected) {
  expect_equal(graph_contents(object), graph_contents(expected))
}

# A node_vec's edges as "from->to" position pairs.
edge_pairs <- function(x) {
  e <- as.data.frame(edges(x))
  paste0(e[["from"]], "->", e[["to"]])
}
