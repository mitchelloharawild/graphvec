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
#' igraph has no concept of a hyperedge (an edge with more than one node in
#' its `from` or `to` role). Converting a `node_vec`/`edge_vec` with a
#' hyperedge column, or an `agg_vec` where a disaggregated value has more
#' than one `<aggregated>` parent, raises an error instead of silently
#' dropping or flattening the extra incidence.
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
  # A forest of stars: each disaggregated row's parents are every aggregated
  # position in the maximal run of `<aggregated>` rows immediately preceding
  # it, up to the next disaggregated row. A run of more than one aggregated
  # position makes that child a hyperedge target (`to` becomes list-valued),
  # since it belongs under all of them at once, not just the nearest.
  is_agg <- is_aggregated(x)
  n <- length(is_agg)
  if (n == 0L) {
    return(igraph_from_edges(from = integer(), to = integer(), n = 0L, directed = TRUE))
  }

  r <- rle(is_agg)
  grp <- rep(seq_along(r$lengths), r$lengths) # run id per position
  nearest_agg <- cummax(seq_len(n) * is_agg) # 0 where no aggregate precedes

  from <- which(!is_agg & nearest_agg > 0L)
  parent_run <- grp[nearest_agg[from]]
  to <- lapply(parent_run, function(g) which(grp == g))
  if (all(lengths(to) == 1L)) to <- as.integer(unlist(to, use.names = FALSE))

  igraph_from_edges(from = from, to = to, n = n, directed = TRUE)
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
  graph <- attr(x, "graph")
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
