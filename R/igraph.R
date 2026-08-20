#' Convert a graph vector to an igraph object
#'
#' Methods for converting `node_vec`, `agg_vec`, `agg_df`, and `edge_vec`
#' objects to [igraph::igraph()] objects using [igraph::graph_from_edgelist()].
#'
#' @param x A `node_vec`, `agg_vec`, `agg_df`, or `edge_vec` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return An [igraph::igraph()] directed graph.
#'
#' @name as.igraph
#' @seealso [node_vec()], [agg_vec()], [agg_df()], [edge_vec()]
NULL

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.agg_vec <- function(x, ...) {
  # A forest of stars: each aggregated position is the parent of the
  # disaggregated rows immediately following it, up to the next aggregate row.
  is_agg <- is_aggregated(x)
  parent <- cummax(seq_along(is_agg) * is_agg)
  from <- which(!is_agg & parent > 0L)
  igraph_from_edges(from = from, to = parent[from], n = length(x))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.agg_df <- function(x, ...) {
  igraph::as.igraph(nodes(x))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.node_vec <- function(x, ...) {
  e <- attr(x, "edges")
  # Vertex count comes from `x`, not the edges, so trailing isolated nodes aren't dropped.
  igraph_from_edges(from = e[["from"]], to = e[["to"]], n = length(x))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.edge_vec <- function(x, ...) {
  e <- edge_vec_data(x)
  igraph_from_edges(
    from = e[["from"]],
    to = e[["to"]],
    n = NROW(attr(x, "nodes"))
  )
}

# Build a directed igraph on exactly `n` vertices, so that nodes without any
# incident edges are preserved.
igraph_from_edges <- function(from, to, n) {
  igraph::add_edges(
    igraph::make_empty_graph(n = n, directed = TRUE),
    rbind(as.integer(from), as.integer(to))
  )
}
