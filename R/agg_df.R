#' Create an aggregation table
#'
#' An `agg_df` is a table of [`agg_vec()`] columns, one row per level of a
#' (possibly crossed) aggregation structure -- e.g. `Purpose` and `State`
#' columns where some rows total one dimension, some the other, some both.
#' [`nodes()`]/[`edges()`] reorient it into a graph: a row is a child of
#' another row whenever the parent aggregates exactly one more column and
#' matches on every other column's disaggregated value.
#'
#' @details
#' An `agg_df`'s rows form a lattice of aggregation levels, with one
#' elementary chain per column (or nested group of columns, e.g.
#' `State/Region` together), combined the way a symbolic formula like
#' `Purpose * (State / Region)` would combine them. See [agg_vec()] for the
#' OLAP `CUBE`/`ROLLUP` correspondence this generalises, and for what the
#' representation deliberately does not support: it is limited to nested
#' and crossed dimension structures inferred by key matching, not a general
#' hypergraph, and a partially-crossed ("pruned"/unbalanced) column
#' combination gives a meet-semilattice missing some joins rather than an
#' error.
#'
#' @param ... Named `agg_vec` columns, all the same length.
#'
#' @return An `agg_df` object.
#'
#' @examples
#' agg_df(
#'   Purpose = agg_vec(c(NA, "Business", "Holiday"), c(TRUE, FALSE, FALSE)),
#'   State = agg_vec(c("NSW", NA, NA), c(FALSE, TRUE, TRUE))
#' )
#'
#' @export
agg_df <- function(...) {
  cols <- list(...)
  nm <- names(cols)
  if (length(cols) > 0 && (is.null(nm) || any(nm == ""))) {
    stop("All columns passed to `agg_df()` must be named.", call. = FALSE)
  }
  if (!all(vapply(cols, inherits, logical(1), what = "agg_vec"))) {
    stop("All columns passed to `agg_df()` must be `agg_vec` objects.", call. = FALSE)
  }
  new_agg_df(cols)
}

# Plain classed list of already-validated agg_vec columns, not a real
# data.frame, so `[`/`c` below get row (not column) semantics.
new_agg_df <- function(cols = list()) {
  sizes <- vapply(cols, length, integer(1))
  if (length(unique(sizes)) > 1) {
    stop("All columns of `agg_df()` must have the same length.", call. = FALSE)
  }
  structure(cols, class = "agg_df")
}

#' @export
length.agg_df <- function(x) {
  if (length(unclass(x)) == 0L) 0L else length(unclass(x)[[1L]])
}

#' @export
format.agg_df <- function(x, ...) {
  do.call(paste, c(lapply(unclass(x), format, ...), list(sep = ":")))
}

#' @export
print.agg_df <- function(x, ...) {
  cat(sprintf("<agg_df[%d]>\n", length(x)))
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

#' @export
`[.agg_df` <- function(x, i, ...) {
  new_agg_df(lapply(unclass(x), `[`, i))
}

#' @export
c.agg_df <- function(...) {
  xs <- list(...)
  if (!all(vapply(xs, inherits, logical(1), what = "agg_df"))) {
    stop("Can only combine `agg_df` objects with other `agg_df` objects.", call. = FALSE)
  }
  nm <- names(unclass(xs[[1]]))
  if (!all(vapply(xs, function(x) identical(names(unclass(x)), nm), logical(1)))) {
    stop("Can't combine `agg_df` objects with different columns.", call. = FALSE)
  }
  new_agg_df(stats::setNames(
    lapply(nm, function(n) do.call(c, lapply(xs, function(x) unclass(x)[[n]]))),
    nm
  ))
}

# Edge table for the aggregation lattice: row i -> row j whenever j
# aggregates exactly one more column than i, matching i on every other
# column's disaggregated value.
agg_lattice_edges <- function(x) {
  cols <- unclass(x)
  p <- length(cols)
  if (p == 0L) {
    return(data.frame(from = integer(), to = integer()))
  }

  n <- length(x)
  agg_mat <- vapply(cols, agg_vec_is_agg, logical(n))
  # Per-column integer keys, grouping on the (aggregated, value) pair so an
  # aggregated cell, a genuine NA and each value are all distinct, and
  # values compare exactly (no character conversion).
  key_mat <- vapply(seq_len(p), function(k) {
    as.integer(vctrs::vec_group_id(vctrs::new_data_frame(list(
      agg = agg_mat[, k], value = agg_vec_expand(cols[[k]])
    ))))
  }, integer(n))

  # Cumulative left-to-right and right-to-left key combinations, so the row
  # key for "every column except j" is the pair of the two halves.
  combine <- function(a, b) {
    as.integer(vctrs::vec_group_id(vctrs::new_data_frame(list(a = a, b = b))))
  }
  empty <- rep(1L, n)
  left <- right <- vector("list", p + 1L)
  left[[1L]] <- empty
  right[[p + 1L]] <- empty
  for (k in seq_len(p)) left[[k + 1L]] <- combine(left[[k]], key_mat[, k])
  for (k in rev(seq_len(p))) right[[k]] <- combine(key_mat[, k], right[[k + 1L]])

  edges <- vector("list", p)
  for (j in seq_len(p)) {
    is_parent <- agg_mat[, j]
    is_child <- !is_parent
    if (!any(is_parent) || !any(is_child)) next

    row_key <- vctrs::new_data_frame(list(l = left[[j]], r = right[[j + 1L]]))
    child_rows <- which(is_child)
    parent_rows <- which(is_parent)
    m <- vctrs::vec_match(
      vctrs::vec_slice(row_key, child_rows),
      vctrs::vec_slice(row_key, parent_rows)
    )
    matched <- !is.na(m)
    edges[[j]] <- data.frame(from = child_rows[matched], to = parent_rows[m[matched]])
  }

  out <- do.call(rbind, edges)
  if (is.null(out)) out <- data.frame(from = integer(), to = integer())
  data.frame(from = as.integer(out$from), to = as.integer(out$to), row.names = NULL)
}

#' @rdname reorient
#' @export
nodes.agg_df <- function(x, ...) {
  new_node_vec(x = x, edges = agg_lattice_edges(x), directed = TRUE)
}

#' @rdname reorient
#' @export
edges.agg_df <- function(x, ...) {
  edges(nodes.agg_df(x, ...))
}
