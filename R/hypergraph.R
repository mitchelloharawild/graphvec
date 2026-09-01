# -- Hyperedge incidence -------------------------------------------------
#
# `from`/`to` are `integer` by default (one node per edge) or a plain list of
# integer vectors when hyperedges are opted into for that column (any number
# of nodes in that role per edge); an ordinary edge is just the length-1
# case. The helpers below let the rest of the package treat both shapes
# uniformly.

# TRUE for a plain integer `from`/`to` column, or a list whose elements are
# all integer vectors (a hyperedge column); FALSE otherwise. A data frame is
# also a list, so it's excluded explicitly even though from/to are never
# actually one.
is_valid_incidence <- function(x) {
  is.integer(x) || (is.list(x) && !is.data.frame(x) && all(vapply(x, is.integer, logical(1))))
}

# Assemble a data.frame from named columns, protecting any hyperedge (list)
# column from data.frame()'s default behaviour of expanding a list-valued
# column argument into one output column per element.
edge_fields_df <- function(cols) {
  cols <- lapply(cols, function(col) if (is.list(col) && !is.data.frame(col)) I(col) else col)
  do.call(data.frame, c(cols, list(stringsAsFactors = FALSE)))
}

# Common-length recycling for from/to/attrs columns. Like data.frame()'s
# built-in recycling, but also works when a hyperedge (list) column needs
# recycling against a longer/shorter plain column -- data.frame() refuses
# that combination outright.
recycle_common <- function(cols) {
  sizes <- vapply(cols, length, integer(1))
  m <- if (length(sizes)) max(sizes) else 0L
  bad <- (sizes == 0L & m > 0L) | (sizes > 0L & m %% sizes != 0L)
  if (any(bad)) {
    stop("`from`, `to`, and edge attributes must be recyclable to a common length.", call. = FALSE)
  }
  lapply(cols, function(col) {
    n <- length(col)
    if (n == m || n == 0L) return(col)
    col[rep(seq_len(n), length.out = m)]
  })
}

# Add `offset` to a from/to field, whether it's a plain integer column or a
# hyperedge (list of integer vectors) one.
offset_incidence <- function(field, offset) {
  if (is.list(field)) I(lapply(field, `+`, offset)) else field + offset
}

# Up-cast a from/to field to a hyperedge (list) column, wrapping each
# ordinary (scalar) position as a length-1 integer vector; a no-op if it's
# already a list. Used to reconcile an ordinary and a hyperedge column when
# combining edge_vec/node_vec objects with c().
as_incidence_list <- function(field) {
  I(lapply(field, as.integer))
}

# Per-edge label for a from/to field, for format()/print(): the ordinary
# node_label() for a ordinary (scalar) column, or one "{a,b}"-style label per
# edge, for a hyperedge column.
incidence_label <- function(nodes, field) {
  if (!is.list(field)) {
    return(node_label(slice_rows(nodes, field)))
  }
  vapply(field, function(idx) {
    lbl <- node_label(slice_rows(nodes, idx))
    if (length(lbl) == 1L) lbl else paste0("{", paste(lbl, collapse = ","), "}")
  }, character(1))
}

# `$from`/`$to` resolution for a from/to field: the node data sliced to the
# field's positions for an ordinary column, or one node-data slice per edge
# (a plain list) for a hyperedge column.
incidence_slice <- function(nodes, field) {
  if (!is.list(field)) {
    return(slice_rows(nodes, field))
  }
  lapply(field, function(idx) slice_rows(nodes, idx))
}

# Every way an edge's from/to membership survives a node reindex (slicing,
# replication, or dropping): `val` is the edge's old position(s) in this
# role -- length 1 for an ordinary column, any length for a hyperedge one --
# and `new_positions[[p]]` lists the new position(s) old position `p` maps
# to (empty if `p` was dropped, several if it was replicated). Returns a
# list of realizations, one integer (ordinary role) or integer vector
# (hyperedge role) per surviving combination; an empty list if any
# referenced node was dropped, meaning the edge doesn't survive at all.
incidence_options <- function(val, new_positions) {
  opts <- lapply(val, function(p) new_positions[[p]])
  if (any(lengths(opts) == 0L)) {
    return(list())
  }
  if (length(opts) == 1L) {
    return(as.list(opts[[1L]]))
  }
  combos <- expand.grid(opts, KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(combos)), function(i) as.integer(combos[i, ]))
}
