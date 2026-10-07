#' Convert a graph vector to an igraph object
#'
#' Methods for converting `node_vec`, `agg_vec`, `agg_df`, and `edge_vec`
#' objects to [igraph::igraph()] objects, keeping their node values and edge
#' attributes.
#'
#' @param x A `node_vec`, `agg_vec`, `agg_df`, or `edge_vec` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return An [igraph::igraph()] object.
#'
#' @details
#' The vertices are the nodes, in order, and carry the node values as vertex
#' attributes, following igraph's convention: a vector of node values
#' becomes the `name` attribute, and each column of a data frame of node
#' values becomes an attribute of the same name. The edges are in order
#' too, with each edge attribute column as an igraph edge attribute. Values
#' keep their type (a factor stays a factor, a date a date). A `node_vec`
#' slice converts to the subgraph its nodes induce, and an `edge_vec` to
#' every node of its graph, with the edges of the slice.
#'
#' An `agg_vec` or `agg_df` is converted as the graph [nodes()] gives it, so
#' `as.igraph(x)` is the same as `as.igraph(nodes(x))`: each row links to
#' the rows that aggregate it (see [agg_df()]), and each column is a vertex
#' attribute holding the column's `agg_vec` (named `value` for an
#' `agg_vec`).
#'
#' igraph has no concept of a hyperedge (an edge with more than one node in
#' its `from` or `to` role). Converting a `node_vec`/`edge_vec` with a
#' hyperedge column raises an error instead of silently dropping or
#' flattening the extra incidence.
#'
#' igraph has no missing edges either, so converting an `edge_vec` with a
#' missing edge (e.g. from `vctrs::vec_init()`) raises an error: drop it
#' first, e.g. with `x[!is.na(x)]`.
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
  edge_attrs <- attr(x, "edges")
  if (!is.null(graph)) {
    e <- graph$edge_endpoints()
  } else {
    e <- edge_attrs
    edge_attrs <- edge_attrs[setdiff(names(edge_attrs), c("from", "to"))]
  }
  igraph_from_edges(
    from = e[["from"]],
    to = e[["to"]],
    n = length(x),
    directed = attr(x, "directed"),
    vertex_attrs = igraph_vertex_attrs(node_vec_data(x)),
    edge_attrs = edge_attrs
  )
}

#' @rdname as.igraph
#' @exportS3Method igraph::as.igraph
as.igraph.edge_vec <- function(x, ...) {
  e <- edge_vec_endpoints(x)
  edge_attrs <- edge_vec_data(x)
  nodes <- attr(x, "nodes")
  igraph_from_edges(
    from = e[["from"]],
    to = e[["to"]],
    n = NROW(nodes),
    directed = attr(x, "directed"),
    vertex_attrs = igraph_vertex_attrs(nodes),
    edge_attrs = edge_attrs[setdiff(names(edge_attrs), c("from", "to"))]
  )
}

# igraph's convention for node values: a vector is the `name` vertex
# attribute, a data frame (or agg_df) gives one vertex attribute per column.
# Values keep their type, so igraph sees what the node_vec holds.
igraph_vertex_attrs <- function(values) {
  if (is.data.frame(values) || inherits(values, "agg_df")) {
    cols <- names(values)
    return(stats::setNames(lapply(cols, function(col) values[[col]]), cols))
  }
  list(name = values)
}

# Build an igraph on exactly `n` vertices, so that nodes without any
# incident edges are preserved. `directed` has no default -- every caller
# must decide and pass it explicitly, so a source of directedness can't be
# silently dropped again. `vertex_attrs` and `edge_attrs` are named lists
# of columns aligned with the vertices and the edges.
igraph_from_edges <- function(from, to, n, directed, vertex_attrs = list(), edge_attrs = list()) {
  if (is.list(from) || is.list(to)) {
    cli::cli_abort(c(
      "x" = "{.pkg igraph} does not support hyperedges.",
      "i" = "Resolve the hyperedge {.field from}/{.field to} column into ordinary edges before calling {.fn as.igraph}."
    ))
  }
  missing <- is.na(from) | is.na(to)
  if (any(missing)) {
    cli::cli_abort(c(
      "x" = "{.pkg igraph} does not support missing edges.",
      "i" = "{sum(missing)} edge{?s} {?is/are} missing, at position{?s} {which(missing)}.",
      "i" = "Drop missing edges (e.g. {.code x[!is.na(x)]}) before calling {.fn as.igraph}."
    ))
  }
  g <- igraph::make_empty_graph(n = n, directed = directed)
  igraph::vertex_attr(g) <- vertex_attrs
  igraph::add_edges(g, rbind(as.integer(from), as.integer(to)), attr = as.list(edge_attrs))
}
