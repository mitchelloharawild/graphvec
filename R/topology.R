# Shared internals for every operation in R/measure.R, R/traverse.R, and
# R/predicate.R.
#
# On the pre-backend design these helpers had to rebuild a CSR adjacency
# index from `from`/`to` on every single call. They don't any more: a
# `node_vec`/`edge_vec` already carries a prebuilt, shared, immutable
# `GraphBackend` in `attr(x, "graph")` (`_dev/RUST_BACKEND.md` §2), so all
# that's left here is (a) reaching that backend from whichever orientation
# `x` arrived in (`_dev/DESIGN.md`'s P2 -- "accept either orientation,
# reorient internally"), (b) the attribute-length checks, and (c) the
# `length(i)` query-dispatch convention.

# The GraphBackend behind `x`, whichever orientation it arrived in.
#
# Plain `if`/`else` on class, not S3 dispatch -- there are exactly two
# concrete inputs, and avoiding UseMethod() here sidesteps roxygen2 flagging
# an unexported "S3 method" that was never meant to be part of the API.
#
# `attr(x, "graph")` is NULL exactly when `x` is hyperedge-shaped
# (list-valued from/to), which the Rust backend does not represent
# (`_dev/RUST_BACKEND.md`) -- so a NULL backend *is* the hyperedge test, and
# no separate inspection of `from`/`to` is needed.
#
# @param x A `node_vec` or `edge_vec`.
# @return The `GraphBackend` external pointer. Errors otherwise.
backend_of <- function(x) {
  if (!inherits(x, "node_vec") && !inherits(x, "edge_vec")) {
    check_node_or_edge_vec(x)
  }
  graph <- attr(x, "graph")
  if (is.null(graph)) {
    check_no_hyperedges()
  }
  if (inherits(x, "node_vec")) {
    # A node_vec slice keeps its parent's whole graph plus the positions of
    # its own nodes in it; its own graph is the induced subgraph on those
    # (free for a node_vec that covers its whole graph).
    graph <- attr(node_vec_compact(x), "graph")
  }
  graph
}

# Shared class guard for operations that work on either orientation. Always
# errors; never returns normally, so a call site can rely on x being one of
# the two classes afterwards.
check_node_or_edge_vec <- function(x) {
  cli::cli_abort(
    "{.arg x} must be a {.cls node_vec} or {.cls edge_vec}, not {.cls {class(x)[1]}}.",
    call = NULL
  )
}

# The topology operations all assume one `from` node and one `to` node per
# edge; a hyperedge (list_of<integer>) column needs an incidence-expanded
# index (`_dev/DESIGN.md` §4.4) that the Rust backend does not build, which
# is why such an object has no backend at all. Always errors.
check_no_hyperedges <- function() {
  cli::cli_abort(c(
    "x" = "This operation does not support hyperedges yet.",
    "i" = "Resolve the hyperedge {.field from}/{.field to} column into ordinary edges first."
  ), call = NULL)
}

# A backend covering *exactly* the edges `x` currently holds.
#
# For a `node_vec` (backend_of() already gives its own, induced, graph), and
# for an `edge_vec` that still spans every edge of its graph in the graph's
# own order, that is the shared backend itself -- free.
#
# A *sliced* `edge_vec` is the interesting case: it keeps its parent graph's
# pointer plus an `edge_id` selection into it (`_dev/RUST_BACKEND.md` §2.2),
# so the shared backend still describes edges the object no longer contains.
# Node-aligned and graph-level answers (degree, neighbours, density, ...)
# have to be about the edges that are actually there -- otherwise
# `node_degree()` would disagree with `n_edges()`, which is `length(x)` --
# so a fresh backend is built over the selected edges. Edge-aligned
# operations don't need this and shouldn't pay for it; they use
# `op_endpoints()` instead.
op_graph <- function(x) {
  graph <- backend_of(x)
  if (!inherits(x, "edge_vec")) {
    return(graph)
  }
  edge_id <- attr(x, "edge_id")
  if (identical(edge_id, seq_len(graph$n_edges()))) {
    return(graph)
  }
  # A missing edge (NA edge_id, e.g. from vctrs::vec_init()) joins no
  # nodes, so it isn't an edge of the graph the operation sees.
  edge_id <- edge_id[!is.na(edge_id)]
  ends <- graph$edge_endpoints()
  graphvec_backend_new(
    graph$n_nodes(),
    ends$from[edge_id],
    ends$to[edge_id],
    attr(x, "directed")
  )
}

# The 1-based (from, to) endpoints of the edges `x` currently holds, in
# `x`'s own edge order -- the alignment every edge-aligned operation needs.
# Never assumes `graph$edge_endpoints()` is already aligned to `x`: a sliced
# `edge_vec` selects into it with `edge_id` (see `edge_vec_endpoints()`).
op_endpoints <- function(x) {
  graph <- backend_of(x)
  if (inherits(x, "edge_vec")) {
    return(edge_vec_endpoints(x))
  }
  graph$edge_endpoints()
}

# The number of edges `x` currently holds. Not `graph$n_edges()`: a sliced
# `edge_vec`'s graph still spans the parent's edges (`op_graph()`).
op_n_edges <- function(x) {
  if (inherits(x, "edge_vec")) {
    return(length(attr(x, "edge_id")))
  }
  backend_of(x)$n_edges()
}

# Two length checks for an attribute vector against `x`'s *current* size --
# no recycling, ever (`_dev/OPERATIONS.md` §2) -- covering the two shapes
# operations need:
#
# - `check_attr_length()`: a *required* node-aligned vector (`values =`),
#   which has no "unweighted" default and so no NULL passthrough.
# - `check_weights_length()`: an *optional* edge-aligned vector
#   (`weights =`) -- every such operation defaults to `weights = NULL` ("run
#   unweighted"), so NULL always passes and the check only fires once a
#   caller actually supplies a vector.
#
# Both take `arg` to name the parameter in the error message, so one call
# site can serve either `weights =` or `values =`.
check_attr_length <- function(attr, n, arg = "values") {
  if (length(attr) != n) {
    cli::cli_abort(
      "{.arg {arg}} must have length {n} (one per node), not {length(attr)}.",
      call = NULL
    )
  }
  invisible(NULL)
}

check_weights_length <- function(weights, m, arg = "weights") {
  if (is.null(weights)) {
    return(invisible(NULL))
  }
  if (length(weights) != m) {
    cli::cli_abort(c(
      "x" = "{.arg {arg}} must have length {m} (the current number of edges in {.arg x}), not {length(weights)}.",
      "i" = "Edge attribute vectors are never recycled."
    ), call = NULL)
  }
  invisible(NULL)
}

# Applies `fn` (a function of one 1-based node/edge position) across `i`:
# a bare vector for a length-1 query, a list of one vector per element of
# `i` otherwise (the package's list_of<integer> shape, `_dev/DESIGN.md`
# §4.4). Dispatching on `length(i)` is what lets `node_neighbors(x, i)`
# vectorise for free without a separate name (`_dev/OPERATIONS.md` §3.1).
query_selection <- function(i, fn) {
  i <- as.integer(i)
  if (length(i) == 1L) fn(i) else lapply(i, fn)
}
