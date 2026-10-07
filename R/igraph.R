#' Convert a graph vector to an igraph object
#'
#' Methods for converting `node_vec`, `agg_vec`, `agg_df`, and `edge_vec`
#' objects to [igraph::igraph()] objects using [igraph::graph_from_edgelist()].
#'
#' @param x A `node_vec`, `agg_vec`, `agg_df`, or `edge_vec` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return An [igraph::igraph()] object.
#'
#' @details
#' An `agg_vec` or `agg_df` is converted as the graph [nodes()] gives it, so
#' `as.igraph(x)` is the same as `as.igraph(nodes(x))`: each row links to
#' the rows that aggregate it (see [agg_df()]).
#'
#' igraph has no concept of a hyperedge (an edge with more than one node in
#' its `from` or `to` role). Converting a `node_vec`/`edge_vec` with a
#' hyperedge column raises an error instead of silently dropping or
#' flattening the extra incidence.
#'
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- node_vec(
#'     x = c("A", "B", "C"),
#'     from = c(1L, 2L),
#'     to = c(2L, 3L)
#'   )
#'   igraph::as.igraph(g)
#' }
#'
#' @name as.igraph
#' @seealso [node_vec()], [agg_vec()], [agg_df()], [edge_vec()]
NULL

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.agg_vec <- function(x, ...) {
  igraph::as.igraph(nodes(x))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.agg_df <- function(x, ...) {
  igraph::as.igraph(nodes(x))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.node_vec <- function(x, ...) {
  # A slice's own edges are the induced subgraph on its nodes.
  x <- node_vec_compact(x)
  graph <- graph_of(x)
  # Node identity is positional, so the vertex count comes from `x` rather than
  # from the edges -- otherwise trailing isolated nodes would be dropped.
  if (!is.null(graph)) {
    e <- graph$edge_endpoints()
    return(igraph_from_edges(from = e$from, to = e$to, n = length(x), directed = attr(x, "directed")))
  }
  e <- attr(x, "edges")
  igraph_from_edges(from = e[["from"]], to = e[["to"]], n = length(x), directed = attr(x, "directed"))
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.edge_vec <- function(x, ...) {
  e <- edge_vec_endpoints(x)
  igraph_from_edges(
    from = e[["from"]],
    to = e[["to"]],
    n = NROW(attr(x, "nodes")),
    directed = attr(x, "directed")
  )
}

# Build an igraph on exactly `n` vertices, so that nodes without any
# incident edges are preserved. `directed` has no default -- every caller
# must decide and pass it explicitly, so a source of directedness can't be
# silently dropped again.
igraph_from_edges <- function(from, to, n, directed) {
  if (is.list(from) || is.list(to)) {
    cli::cli_abort(c(
      "x" = "{.pkg igraph} does not support hyperedges.",
      "i" = "Resolve the hyperedge {.field from}/{.field to} column into ordinary edges before calling {.fn as.igraph}."
    ))
  }
  igraph::add_edges(
    igraph::make_empty_graph(n = n, directed = directed),
    rbind(as.integer(from), as.integer(to))
  )
}
