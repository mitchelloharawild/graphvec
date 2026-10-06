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

# Dense rank of the rows of `cols`, a list of key columns (data frame columns
# included, compared column by column), sorting lexicographically like
# vctrs::vec_rank(ties = "dense"): missing values last within each column
# and characters in the C locale. A row is NA only when every key is missing.
rank_rows <- function(cols, n) {
  keys <- flatten_keys(cols)
  if (length(keys) == 0L) {
    return(rep(1L, n))
  }
  ord <- do.call(order, c(unname(keys), list(na.last = TRUE, method = "radix")))
  # A new rank starts wherever any key differs from the row sorted before it.
  new_rank <- rep(n > 0L, n)
  if (n > 1L) {
    new_rank[-1L] <- Reduce(`|`, lapply(keys, function(k) {
      s <- k[ord]
      prev <- s[-n]
      cur <- s[-1L]
      same <- (cur == prev) | (is.na(cur) & is.na(prev))
      is.na(same) | !same
    }))
  }
  out <- integer(n)
  out[ord] <- cumsum(new_rank)
  out[Reduce(`&`, lapply(keys, is.na))] <- NA_integer_
  out
}

# Splices data frame columns into a flat list of keys that order() can sort.
flatten_keys <- function(cols) {
  unlist(lapply(cols, function(col) {
    if (is.data.frame(col)) flatten_keys(col) else list(xtfrm(col))
  }), recursive = FALSE)
}

# `[[` selects exactly one element, by a single position.
check_scalar_index <- function(i) {
  if (length(i) != 1L || is.na(i)) {
    stop("Can't select more or less than one element with `[[`.", call. = FALSE)
  }
}
