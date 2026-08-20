#' Create an aggregation vector
#'
#' An aggregation vector is a special type of [`node_vec()`] consisting of a
#' single parent (the 'aggregated' value) and its children. Aggregated values
#' are identified by a logical vector passed to the `aggregated` argument, and
#' disaggregated values are provided in `x`. Aggregated values are displayed
#' as `<aggregated>` by default.
#'
#' @param x The vector of values.
#' @param aggregated A logical vector to identify which values are `<aggregated>`.
#'
#' @return An `agg_vec` object.
#'
#' @examples
#' agg_vec(
#'   x = c(NA, "A", "B"),
#'   aggregated = c(TRUE, FALSE, FALSE)
#' )
#'
#' @export
agg_vec <- function(x = character(), aggregated = logical(NROW(x))){
  is_agg <- is_aggregated(x)
  if (inherits(x, "agg_vec")) x <- agg_vec_expand(x)
  stopifnot(is.logical(aggregated))
  is_agg <- is_agg | aggregated
  new_agg_vec(x[!is_agg], which(is_agg))
}

# x: disaggregated values. agg_pos: their aggregated positions in the full
# vector. x is wrapped in a list so its class/attributes aren't overwritten.
new_agg_vec <- function(x, agg_pos) {
  structure(list(x), class = "agg_vec", agg_pos = agg_pos)
}

# The disaggregated values, unwrapped.
agg_vec_values <- function(x) {
  x[[1L]]
}

# Full-length logical mask: TRUE at each aggregated position.
agg_vec_is_agg <- function(x) {
  out <- logical(length(agg_vec_values(x)) + length(attr(x, "agg_pos")))
  out[attr(x, "agg_pos")] <- TRUE
  out
}

# Full-length vector: real values at disaggregated positions, NA at aggregated ones.
agg_vec_expand <- function(x) {
  is_agg <- agg_vec_is_agg(x)
  vals <- agg_vec_values(x)
  out <- vals[rep(NA_integer_, length(is_agg))]
  out[!is_agg] <- vals
  out
}

#' @export
format.agg_vec <- function(x, ..., agg_chr = "<aggregated>"){
  is_agg <- agg_vec_is_agg(x)
  out <- character(length(is_agg))
  out[is_agg] <- agg_chr
  out[!is_agg] <- format(agg_vec_values(x), ...)
  out
}

#' @export
print.agg_vec <- function(x, ...) {
  cat(sprintf("<agg_vec[%d]>\n", length(x)))
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

# Registered dynamically for pillar via zzz.R.
pillar_shaft.agg_vec <- function(x, ...) {
  if(requireNamespace("crayon", quietly = TRUE)){
    agg_chr <- crayon::style("<aggregated>", crayon::make_style("#999999", grey = TRUE))
  }
  else{
    agg_chr <- "<aggregated>"
  }

  out <- format(x, agg_chr = agg_chr)

  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 10)
}

# Registered dynamically for pillar via zzz.R; abbreviated type header, e.g. "chr*".
type_sum.agg_vec <- function(x, ...) {
  paste0(pillar::type_sum(agg_vec_values(x)), "*")
}

#' @export
length.agg_vec <- function(x) {
  length(agg_vec_values(x)) + length(attr(x, "agg_pos"))
}

#' @export
`[.agg_vec` <- function(x, i, ...) {
  is_agg <- agg_vec_is_agg(x)[i]
  vals <- agg_vec_expand(x)[i]
  new_agg_vec(vals[!is_agg], which(is_agg))
}

#' @export
c.agg_vec <- function(...) {
  xs <- list(...)
  sizes <- vapply(xs, length, integer(1))
  offsets <- cumsum(c(0L, utils::head(sizes, -1L)))
  new_agg_vec(
    x = do.call(c, lapply(xs, agg_vec_values)),
    agg_pos = do.call(c, Map(function(x, offset) attr(x, "agg_pos") + offset, xs, offsets))
  )
}

#' @export
`==.agg_vec` <- function(e1, e2){
  e1_agg <- inherits(e1, "agg_vec")
  e2_agg <- inherits(e2, "agg_vec")

  if(!e1_agg || !e2_agg){
    x <- list(e1,e2)[[which(!c(e1_agg, e2_agg))]]
    is_agg <- x == "<aggregated>"
    if(any(is_agg)){
      warning("<aggregated> character values have been converted to aggregated values.
Hint: If you're trying to compare aggregated values, use `is_aggregated()`.")
    }
    x <- agg_vec(ifelse(is_agg, NA, x), aggregated = is_agg)
    if(!e1_agg) e1 <- x else e2 <- x
  }

  x1 <- agg_vec_expand(e1)
  x2 <- agg_vec_expand(e2)
  val_eq <- (x1 == x2) | (is.na(x1) & is.na(x2))
  val_eq[is.na(val_eq)] <- FALSE
  (agg_vec_is_agg(e1) & agg_vec_is_agg(e2)) | val_eq
}

#' @export
`!=.agg_vec` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
is.na.agg_vec <- function(x) {
  is.na(agg_vec_expand(x)) & !agg_vec_is_agg(x)
}

# 1-column special case: every aggregated position is a parent of every
# non-aggregated position (a single star).
#' @rdname reorient
#' @export
nodes.agg_vec <- function(x, ...) {
  nodes(new_agg_df(list(value = x)))
}

#' @rdname reorient
#' @export
edges.agg_vec <- function(x, ...) {
  edges(new_agg_df(list(value = x)))
}

# #' @importFrom dplyr recode
# #' @export
# recode.agg_vec <- function(.x, ...) {
#   field(.x, "x") <- recode(field(.x, "x"), ...)
#   .x
# }

#' Is the element an aggregation of smaller data
#'
#' @param x An object.
#' @return A logical vector indicating which elements are aggregated.
#'
#' @seealso [`agg_vec()`]
#'
#' @export
is_aggregated <- function(x){
  if(!inherits(x, "agg_vec")){
    logical(NROW(x))
  } else {
    agg_vec_is_agg(x)
  }
}
