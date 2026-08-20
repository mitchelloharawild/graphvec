# Row-slice `x` by position; a bare `[` index selects columns, not rows, on
# a data frame, so branch on shape explicitly.
slice_rows <- function(x, i) {
  if (is.data.frame(x)) {
    out <- x[i, , drop = FALSE]
    rownames(out) <- NULL
    out
  } else {
    x[i]
  }
}

# Combine several node/value vectors end to end, in order.
combine_values <- function(xs) {
  if (all(vapply(xs, is.data.frame, logical(1)))) {
    rbind_fill(xs)
  } else {
    do.call(c, xs)
  }
}

# Row-bind data frames that may have different columns, padding any column
# missing from one frame with NA in the rows contributed by that frame.
rbind_fill <- function(dfs) {
  all_names <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(d) {
    for (nm in setdiff(all_names, names(d))) d[[nm]] <- NA
    d[all_names]
  })
  do.call(rbind, dfs)
}
