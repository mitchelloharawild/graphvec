#' Convert a graph vector to an igraph object
#'
#' Methods for converting `node_vec`, `agg_vec`, and `edge_vec` objects to
#' [igraph::igraph()] objects using [igraph::graph_from_edgelist()].
#'
#' @param x A `node_vec`, `agg_vec`, or `edge_vec` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return An [igraph::igraph()] directed graph.
#'
#' @name as.igraph
#' @seealso [node_vec()], [agg_vec()], [edge_vec()]
NULL

#' @rdname as.igraph
as.igraph.agg_vec <- function(x, ...) {
  e <- attr(x, "edges")
  igraph::graph_from_edgelist(
    cbind(from = unlist(e[["from"]]), to = rep(e[["to"]], lengths(e[["from"]])))
  )
}

#' @rdname as.igraph
as.igraph.node_vec <- function(x, ...) {
  e <- attr(x, "edges")
  igraph::graph_from_edgelist(
    cbind(from = unlist(e[["from"]]), to = rep(e[["to"]], lengths(e[["from"]])))
  )
}

#' @rdname as.igraph
as.igraph.edge_vec <- function(x, ...) {
  e <- vec_data(x)
  igraph::graph_from_edgelist(
    cbind(from = e[["from"]], to = e[["to"]])
  )
}
