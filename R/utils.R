# Row-slice `x` (any node/value vector, including a data frame of node
# attributes) by position. A bare single-bracket index on a data frame
# selects columns, not rows, so branch on shape explicitly rather than using
# `[` uniformly; for anything else, `x[i]` already does the right thing via
# x's own `[` method (e.g. `[.factor`, `[.agg_vec`).
slice_rows <- function(x, i) {
  if (is.data.frame(x)) {
    out <- x[i, , drop = FALSE]
    rownames(out) <- NULL
    out
  } else {
    x[i]
  }
}

# Combine several node/value vectors end to end, in order -- the same
# data-frame-vs-everything-else branch as slice_rows(), since rbind()/c()
# split the same way base R already splits `[`. Used to combine `x` across
# several node_vecs (c.node_vec()) and `nodes` across several edge_vecs
# (c.edge_vec()).
combine_values <- function(xs) {
  if (all(vapply(xs, is.data.frame, logical(1)))) {
    rbind_fill(xs)
  } else {
    do.call(c, xs)
  }
}

# Row-bind data frames that may have different columns, padding any column
# missing from one frame with NA in the rows contributed by that frame --
# used for edge/node attribute schemas that differ across the node_vec/
# edge_vec objects being combined.
rbind_fill <- function(dfs) {
  all_names <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(d) {
    for (nm in setdiff(all_names, names(d))) d[[nm]] <- NA
    d[all_names]
  })
  do.call(rbind, dfs)
}
