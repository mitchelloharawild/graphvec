#' Node position predicates
#'
#' Free O(1)-per-node predicates on [node_degree()]: a node with no incident
#' edges at all, no incoming edge, or no outgoing edge, respectively. For an
#' undirected graph, `node_is_root()`/`node_is_leaf()` coincide with
#' `node_is_isolated()` -- in- and out-degree are the same thing there, so
#' "no incoming edge" and "no incident edge" mean the same thing.
#'
#' A self-loop makes its node non-isolated (and neither a root nor a leaf in
#' the directed case), since it is an incident edge like any other.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#'
#' @return A logical vector of length `n_nodes(x)`.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' node_is_isolated(g)
#' node_is_root(g)
#' node_is_leaf(g)
#'
#' @export
node_is_isolated <- function(x) {
  node_degree(x, mode = "all") == 0L
}

#' @rdname node_is_isolated
#' @export
node_is_root <- function(x) {
  node_degree(x, mode = "in") == 0L
}

#' @rdname node_is_isolated
#' @export
node_is_leaf <- function(x) {
  node_degree(x, mode = "out") == 0L
}

#' Edge position predicates
#'
#' `edge_is_loop()` flags a self-loop (`from == to`), free.
#' `edge_multiplicity()` counts, for each edge, how many edges share its
#' endpoints (the same ordered pair for a directed graph, the same unordered
#' pair otherwise); `edge_is_multi()` flags an edge with multiplicity
#' greater than one, i.e. one with at least one parallel duplicate. Both are
#' O(M).
#'
#' Multiplicity is counted among the edges `x` *currently* holds: an
#' `edge_vec` sliced down to one of a pair of parallel edges has
#' multiplicity 1, matching `n_edges(x)` and `length(x)`. A missing edge
#' (e.g. from `vctrs::vec_init()`) has no endpoints, so its multiplicity is
#' `NA` and it is not parallel to any edge, not even another missing one.
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#'
#' @return A logical vector of length `n_edges(x)`, except
#' `edge_multiplicity()`, which returns an integer vector of the same
#' length.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 1L, 3L), to = c(2L, 2L, 3L))
#' edge_is_loop(g)
#' edge_multiplicity(g)
#' edge_is_multi(g)
#'
#' @export
edge_is_loop <- function(x) {
  ends <- op_endpoints(x)
  ends$from == ends$to
}

#' @rdname edge_is_loop
#' @export
edge_multiplicity <- function(x) {
  directed <- backend_of(x)$is_directed()
  ends <- op_endpoints(x)
  # A missing edge (e.g. from `vec_init()`) has no endpoints, so it is no
  # edge's parallel: its multiplicity is NA and it is left out of the groups.
  ok <- !(is.na(ends$from) | is.na(ends$to))
  from <- ends$from[ok]
  to <- ends$to[ok]
  if (!directed) {
    lo <- pmin(from, to)
    to <- pmax(from, to)
    from <- lo
  }
  # One group id per (from, to) pair, so tabulating the ids counts each
  # group once -- O(M) hashing, without string keys or table()'s factor
  # construction and level sort.
  group <- vctrs::vec_group_id(vctrs::new_data_frame(list(from = from, to = to)))
  out <- rep(NA_integer_, length(ok))
  out[ok] <- tabulate(group, nbins = attr(group, "n"))[group]
  out
}

#' @rdname edge_is_loop
#' @export
edge_is_multi <- function(x) {
  edge_multiplicity(x) > 1L
}

#' Whole-graph predicates
#'
#' `graph_is_directed()` reads the `directed` flag `x` was built with -- is
#' incidence ordered (`from` -> `to`) or symmetric? `graph_has_loops()` is
#' `any(edge_is_loop(x))`: does any edge join a node to itself?
#'
#' @param x A `node_vec` or `edge_vec`. Either orientation is accepted and
#' reoriented internally. Hyperedges are not yet supported.
#'
#' @return A single logical.
#'
#' @examples
#' g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
#' graph_is_directed(g)
#' graph_has_loops(g)
#'
#' looped <- node_vec(x = c("A", "B"), from = 1L, to = 1L)
#' graph_has_loops(looped)
#'
#' @export
graph_is_directed <- function(x) {
  backend_of(x)$is_directed()
}

#' @rdname graph_is_directed
#' @export
graph_has_loops <- function(x) {
  any(edge_is_loop(x))
}
