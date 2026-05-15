as.igraph.agg_vec <- function(x) {
  e <- attr(x, "edges")
  igraph::graph_from_edgelist(
    cbind(from = unlist(e[["from"]]), to = rep(e[["to"]], lengths(e[["from"]])))
  )
}

as.igraph.node_vec <- function(x) {
  e <- attr(x, "edges")
  igraph::graph_from_edgelist(
    cbind(from = unlist(e[["from"]]), to = rep(e[["to"]], lengths(e[["from"]])))
  )
}

as.igraph.edge_vec <- function(x) {
  e <- vec_data(x)
  igraph::graph_from_edgelist(
    cbind(from = e[["from"]], to = e[["to"]])
  )
}
