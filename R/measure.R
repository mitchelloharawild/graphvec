#' Node degree
#'
#' The number of edges incident to each node, for every node at once.
#'
#' Answered directly by the shared `GraphBackend` (`_dev/RUST_BACKEND.md`):
#' one O(1) lookup per node for `"out"`/`"in"`, and for `"all"` on a
#' directed graph, from the degree counts cached when the graph was built --
#' O(N) overall, with no adjacency index to rebuild per call.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#' @param mode For a directed graph: `"all"` (in- plus out-degree), `"out"`,
#' or `"in"`. Ignored for an undirected graph, where in- and out-degree
#' coincide.
#'
#' @section Self-loops:
#' In an **undirected** graph a self-loop counts **once**, matching the
#' backend's own convention (and 'petgraph''s `neighbors_undirected()`) --
#' one entry per incident edge, so `node_degree()` always equals
#' `length(node_neighbors())`. Note that this differs from 'igraph', which
#' follows the textbook "a self-loop adds 2 to the undirected degree"
#' convention. In a **directed** graph a self-loop contributes 1 to the
#' out-degree and 1 to the in-degree, so 2 under `mode = "all"`.
#'
#' @return An integer vector of length `n_nodes(x)`.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' node_degree(g)
#' node_degree(g, mode = "out")
#'
#' @export
node_degree <- function(x, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  graph <- op_graph(x)
  # `graph$degree` is resolved once, not once per node: `$.GraphBackend`
  # rebuilds the closure (and rebinds `self`) on every access, so hoisting
  # it out of the loop is what keeps this a single .Call per node.
  degree <- graph$degree
  vapply(seq_len(graph$n_nodes()), degree, integer(1), mode = mode)
}

#' Node and edge counts
#'
#' The number of nodes/edges in `x`, kept bare rather than `graph_`-prefixed
#' -- the "ask either orientation" counts tied to `length()`
#' (`_dev/OPERATIONS.md` §1 rule 3). Both are O(1).
#'
#' `n_edges()` counts the edges `x` *currently* holds, so it always agrees
#' with `length(edges(x))`: an `edge_vec` sliced away from its graph keeps
#' the shared graph pointer but only a selection of its edges, and it is the
#' selection that counts.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#'
#' @return A single integer.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' n_nodes(g)
#' n_edges(g)
#'
#' @export
n_nodes <- function(x) {
  backend_of(x)$n_nodes()
}

#' @rdname n_nodes
#' @export
n_edges <- function(x) {
  op_n_edges(x)
}

#' Graph density
#'
#' The fraction of possible edges that are actually present: `M` over the
#' number of ordered pairs (directed) or unordered pairs (undirected) of
#' distinct nodes. Self-loops and parallel edges still count towards `M`, so
#' a multigraph can exceed a density of 1.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#'
#' @return A single double, or `NaN` for a graph with fewer than 2 nodes (no
#' possible edges to take a fraction of).
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' graph_density(g)
#'
#' @export
graph_density <- function(x) {
  graph <- backend_of(x)
  n <- graph$n_nodes()
  if (n <= 1L) {
    return(NaN)
  }
  m <- op_n_edges(x)
  denom <- if (graph$is_directed()) n * (n - 1) else n * (n - 1) / 2
  m / denom
}
