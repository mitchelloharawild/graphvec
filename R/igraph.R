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
#' @exportS3Method igraph::as.igraph
as.igraph.agg_vec <- function(x, ...) {
  # agg_vec never stores an edge table (DESIGN.md 8.3): within one star,
  # `from` is every disaggregated position and `to` is that star's
  # aggregate position, computed on demand rather than materialised into a
  # node_vec. A panel is a forest of such stars -- `aggregated` may be TRUE
  # at more than one position, each the root of its own disjoint star, with
  # the disaggregated rows immediately following it (up to the next
  # aggregate row) as its children.
  is_agg <- is_aggregated(x)
  parent <- cummax(seq_along(is_agg) * is_agg)
  from <- which(!is_agg & parent > 0L)
  igraph_from_edges(from = from, to = parent[from], n = length(x))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.node_vec <- function(x, ...) {
  e <- attr(x, "edges")
  # Node identity is positional, so the vertex count comes from `x` rather than
  # from the edges -- otherwise trailing isolated nodes would be dropped.
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
