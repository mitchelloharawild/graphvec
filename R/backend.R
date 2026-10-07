# -- Rust GraphBackend glue -----------------------------------------------
#
# Thin R-side helpers around the compiled `GraphBackend` external pointer
# (src/rust/src/lib.rs) that backs node_vec/edge_vec's non-hyperedge
# storage (_dev/RUST_BACKEND.md). Hyperedge-shaped graphs (list-valued
# from/to) never touch this file -- they keep the pure-R
# `edges`/`nodes`-attribute representation exactly as before.

# Build a GraphBackend on `n` nodes from 1-based from/to positions. Every
# position must be a node, `1..n`: the constructors check that first with
# check_edge_positions(), and the Rust side errors on anything that gets
# past them rather than indexing out of range.
graphvec_backend_new <- function(n, from, to, directed) {
  GraphBackend$new(as.integer(n), as.integer(from), as.integer(to), as.logical(directed))
}

# Errors unless every non-missing `from`/`to` position is a node of a graph
# on `n` nodes, i.e. in `1..n`. `n = Inf` (an edge_vec without node data,
# whose node count is inferred from the positions themselves) checks only
# that they're positive. Missing positions are left to the caller: a
# missing edge in an edge_vec, but an error in a node_vec.
check_edge_positions <- function(from, to, n) {
  pos <- c(from, to)
  bad <- unique(pos[!is.na(pos) & (pos < 1L | pos > n)])
  if (length(bad) == 0L) {
    return(invisible(NULL))
  }
  bad <- utils::head(bad, 5L)
  if (is.infinite(n)) {
    cli::cli_abort(c(
      "{.arg from} and {.arg to} must be positive node positions.",
      "x" = "Found {.val {bad}}."
    ), call = NULL)
  }
  cli::cli_abort(c(
    "{.arg from} and {.arg to} must be node positions between 1 and {n}.",
    "x" = "Found {.val {bad}}."
  ), call = NULL)
}

# Induced-subgraph remap, translating R's NA_integer_ "no source" sentinel
# to the 0 the Rust side expects -- NA doesn't survive the R -> Rust integer
# conversion (it becomes .Machine$integer.min), so this substitution has to
# happen on the R side, not in Rust.
graphvec_backend_induced_subgraph <- function(graph, idx) {
  idx <- as.integer(idx)
  idx[is.na(idx)] <- 0L
  graph$induced_subgraph(idx)
}

# A data frame of `n` rows and zero columns from a possibly-empty named list
# of equal-length columns. `data.frame()` alone loses the row count when the
# list is empty (yielding a 0x0 frame), which happens whenever an
# edge-attribute-free edge_vec/node_vec has its `from`/`to` columns excluded
# to build the attribute-only table that sits alongside `graph`.
attrs_frame <- function(attrs, n) {
  if (length(attrs) == 0L) {
    return(data.frame(row.names = seq_len(n))[, character(0), drop = FALSE])
  }
  edge_fields_df(attrs)
}

# Reassemble a full from/to/attrs data frame -- the shape edge_vec/node_vec
# used to store directly before this backend existed, still needed as a
# common currency for c()'s hyperedge up-casting and igraph conversion.
# `attrs` may be NULL or a zero-column data frame.
cbind_edge_fields <- function(from, to, attrs = NULL) {
  base <- edge_fields_df(list(from = from, to = to))
  # length(), not NCOL()/ncol(): `attrs` is frequently a class-stripped
  # edge_vec body (edge_vec_data()) which, having lost its "data.frame"
  # class, no longer reports a meaningful nrow()/NCOL() even though its
  # row.names attribute is technically still there -- length() (the column
  # count) is unaffected by that and works the same whether or not `attrs`
  # is classed as a data.frame.
  if (is.null(attrs) || length(attrs) == 0L) return(base)
  cbind(base, attrs)
}
