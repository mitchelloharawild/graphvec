#' Neighboring nodes
#'
#' The nodes adjacent to node `i`, answered in O(d) by the shared
#' `GraphBackend`'s adjacency (`d` being the degree of `i`) -- no per-call
#' index build, unlike the O(M) scan a bare edge list would need.
#'
#' `node_parents()`/`node_children()` are the `mode = "in"`/`mode = "out"`
#' aliases, named for the directed reading.
#'
#' One entry is returned **per incident edge**, not per distinct neighbour,
#' so parallel edges produce repeated positions and
#' `length(node_neighbors(x, i))` is `node_degree(x)[i]`. Positions come
#' back in increasing order: the backend's own iteration order depends on
#' which physical representation the graph picked
#' (`_dev/petgraph_data_types.md`), so it is sorted here to keep the result
#' reproducible. It is therefore *not* positionally paired with
#' [edge_incident()], which is ordered by edge position.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#' @param i One or more node positions to query.
#' @param mode For a directed graph: `"out"` (successors), `"in"`
#' (predecessors), or `"all"` (both -- possibly with duplicates if `i` and
#' another node are joined in both directions). Ignored for an undirected
#' graph, where every incident edge is symmetric; an undirected self-loop
#' appears once (see [node_degree()] on self-loops).
#'
#' @return An integer vector of node positions when `i` has length 1; a list
#' of integer vectors (one per element of `i`) otherwise.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' node_neighbors(g, 2)
#' node_neighbors(g, 1:3)
#' node_children(g, 1)
#'
#' @export
node_neighbors <- function(x, i, mode = c("out", "in", "all")) {
  mode <- match.arg(mode)
  graph <- op_graph(x)
  neighbors <- graph$neighbors
  query_selection(i, function(pos) sort(neighbors(pos, mode)))
}

#' @rdname node_neighbors
#' @export
node_parents <- function(x, i) {
  node_neighbors(x, i, mode = "in")
}

#' @rdname node_neighbors
#' @export
node_children <- function(x, i) {
  node_neighbors(x, i, mode = "out")
}

#' Edges touching a node
#'
#' The edges incident to node `i`, in increasing edge position, with the
#' same `mode` and self-loop handling as [node_neighbors()] -- so
#' `length(edge_incident(x, i))` is `node_degree(x)[i]` too.
#'
#' The backend exposes an adjacency over *nodes*, not over edge ids, so this
#' is an O(M) scan of the edge endpoints per query rather than
#' [node_neighbors()]'s O(d) lookup.
#'
#' @inheritParams node_neighbors
#'
#' @return An integer vector of edge positions when `i` has length 1; a list
#' of integer vectors (one per element of `i`) otherwise.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' edge_incident(g, 2, mode = "all")
#'
#' @export
edge_incident <- function(x, i, mode = c("out", "in", "all")) {
  mode <- match.arg(mode)
  graph <- backend_of(x)
  ends <- op_endpoints(x)
  directed <- graph$is_directed()
  query_selection(i, function(pos) {
    if (!directed) {
      # One symmetric incidence: an edge touching `pos` in either role is
      # listed once, self-loop included (it is one edge, and the undirected
      # convention counts it once).
      return(which(ends$from == pos | ends$to == pos))
    }
    out <- if (mode != "in") which(ends$from == pos) else integer()
    inn <- if (mode != "out") which(ends$to == pos) else integer()
    sort(c(out, inn))
  })
}

#' Endpoint nodes of an edge
#'
#' The nodes touching edge `i` -- its `from` and `to` positions, in that
#' order (a self-loop returns the same position twice). O(1) per edge: an
#' edge's own endpoints are stored directly, so no adjacency lookup is
#' involved.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#' @param i One or more edge positions to query.
#'
#' @return An integer vector of length 2 (`c(from, to)`) when `i` has length
#' 1; a list of such vectors (one per element of `i`) otherwise.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' node_incident(g, 1)
#' node_incident(g, 1:2)
#'
#' @export
node_incident <- function(x, i) {
  ends <- op_endpoints(x)
  query_selection(i, function(pos) c(ends$from[pos], ends$to[pos]))
}

#' Edge endpoints, edge-aligned
#'
#' The `from`/`to` node of every edge, aligned to the edge axis (length
#' `n_edges(x)`) -- `e$from`/`e$to` restated as functions so they're
#' reachable from either orientation, not just an `edge_vec`. O(M).
#'
#' Values are node *identities* (the node data itself), not positions --
#' use [node_incident()] for positions.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#'
#' @return A vector of node identities, one per edge.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' edge_tails(g)
#' edge_heads(g)
#'
#' @export
edge_heads <- function(x) {
  backend_of(x)
  edges(x)$to
}

#' @rdname edge_heads
#' @export
edge_tails <- function(x) {
  backend_of(x)
  edges(x)$from
}
