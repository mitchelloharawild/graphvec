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
    for (nm in setdiff(all_names, names(d))) d[[nm]] <- rep(NA, nrow(d))
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

# The positions `[` selects with `i`, as for a base vector: a name selects
# the first element with that name, and an unknown name, `NA` or a position
# past the end gives a missing (`NA`) position.
subscript_positions <- function(x, i) {
  if (is.character(i)) {
    return(match(i, names(x), incomparables = c(NA, "")))
  }
  seq_len(length(x))[i]
}

# Extracting with `[[` also needs an existing element, as for base vectors,
# rather than `[`'s missing value or empty result. Returns the element's
# position, matching a name to its first position as base `[[` does.
element_position <- function(x, i) {
  check_scalar_index(i)
  pos <- if (is.character(i)) {
    match(i, names(x))
  } else if (is.numeric(i) && i >= 1 && i < length(x) + 1) {
    as.integer(i)
  } else {
    NA_integer_
  }
  if (is.na(pos)) {
    stop("Can't extract element ", format(i), " with `[[`: subscript out of bounds.", call. = FALSE)
  }
  pos
}

# The value anyDuplicated() returns given duplicated()'s result: the
# position of the first duplicate, scanning from the end with `fromLast`,
# or 0 if there is none.
first_duplicate <- function(dup, fromLast = FALSE, ...) {
  pos <- which(dup)
  if (length(pos) == 0L) {
    return(0L)
  }
  if (isTRUE(fromLast)) max(pos) else min(pos)
}

# One string key per row of the per-row fields `cols` (a list of atomic
# vectors, data frames and lists of per-row vectors), identical for two
# rows exactly when vctrs::vec_equal(na_equal = TRUE) finds every field
# equal. Each row is keyed on its own, so the keys of separate vectors
# compare too, as base match() needs of mtfrm(). Every token delimits
# itself (numbers have no `|`, `(` or `[`; strings are length-prefixed), so
# different rows can't run together into the same key.
equality_key <- function(cols, n) {
  if (length(cols) == 0L) {
    return(rep("", n))
  }
  do.call(paste, c(unname(lapply(cols, key_column)), sep = "|"))
}

key_column <- function(col) {
  if (is.data.frame(col)) {
    return(paste0("(", equality_key(as.list(col), nrow(col)), ")"))
  }
  if (is.list(col)) {
    return(vapply(col, key_element, character(1)))
  }
  # Factors compare by label, as vctrs casts them to common levels first.
  if (is.factor(col)) {
    col <- as.character(col)
  }
  col <- vctrs::vec_proxy_equal(col)
  if (is.data.frame(col)) {
    return(key_column(col))
  }
  switch(typeof(col),
    character = {
      col <- enc2utf8(col)
      ifelse(is.na(col), "NA", paste0(nchar(col, "bytes"), ":", col))
    },
    complex = paste0(key_number(Re(col)), "i", key_number(Im(col))),
    # Logical, integer, double and raw all as doubles, as vctrs casts them
    # to a common type before comparing.
    key_number(col)
  )
}

# A list element (e.g. a hyperedge's node set): NULL is its missing value.
key_element <- function(v) {
  if (is.null(v)) {
    return("NULL")
  }
  paste0("[", paste(key_column(v), collapse = ","), "]")
}

# Doubles exactly, in hexadecimal: NA and NaN differ, 0 and -0 don't.
key_number <- function(x) {
  x <- as.double(x)
  x[!is.na(x) & x == 0] <- 0
  sprintf("%a", x)
}

# For each element of the group ids `g` (positive integers), how many times
# its group has occurred so far: 1 for the first, 2 for the second, ...
occurrence <- function(g) {
  out <- integer(length(g))
  out[order(g)] <- sequence(tabulate(g))
  out
}
