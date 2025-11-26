#' @method as.igraph node_vec
#' @export
#' @importFrom igraph as.igraph graph_from_edgelist
as.igraph.node_vec <- function(x) {
  # Temporary design hack to allow for direct passing of combined graph
  # if(!all(c("from", "to") %in% names(g))) {
  #   if(inherits(g, c("agg_vec", "node_vec"))) {
  #     g <- list(g)
  #   }
  #   g <- combine_graph(g)
  # }

  e <- attr(x, "edges")

  graph_from_edgelist(
    cbind(from = unlist(e[["from"]]), to = rep(e[["to"]], lengths(e[["from"]])))
  )
}

#' @method as.igraph edge_vec
#' @export
#' @importFrom igraph as.igraph graph_from_edgelist
as.igraph.edge_vec <- function(x) {
  e <- vec_data(x)
  graph_from_edgelist(
    cbind(from = e[["from"]], to = e[["to"]])
  )
}
