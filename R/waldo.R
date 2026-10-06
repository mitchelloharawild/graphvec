# waldo compatibility methods
#
# Equivalent node_vecs/edge_vecs hold different GraphBackend pointers, which
# waldo compares by address, so testthat's expect_equal() would never find
# them equal. These proxy each vector by its contents instead. waldo is only
# suggested, so they're registered dynamically via zzz.R.

compare_proxy.node_vec <- function(x, path) {
  edges <- node_vec_full_edges(x)
  rownames(edges) <- NULL
  list(
    object = list(
      class = class(x),
      nodes = node_vec_data(x),
      edges = edges,
      directed = attr(x, "directed")
    ),
    path = paste0("graph_contents(", path, ")")
  )
}

compare_proxy.edge_vec <- function(x, path) {
  edges <- edge_vec_fields_df(x)
  rownames(edges) <- NULL
  list(
    object = list(
      class = class(x),
      nodes = attr(x, "nodes"),
      edges = edges,
      directed = attr(x, "directed")
    ),
    path = paste0("graph_contents(", path, ")")
  )
}
