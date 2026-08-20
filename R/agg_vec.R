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
  if (inherits(x, "agg_vec")) x <- x[["x"]]
  x[is_agg] <- NA
  stopifnot(is.logical(aggregated))
  new_agg_vec(x, is_agg | aggregated)
}

# Low-level constructor: `x` and `agg` are already the same length, no
# validation or aggregated-value clearing (agg_vec() above does that).
new_agg_vec <- function(x, agg) {
  structure(list(x = x, agg = agg), class = "agg_vec")
}

#' @export
format.agg_vec <- function(x, ..., agg_chr = "<aggregated>"){
  is_agg <- x[["agg"]]
  out <- character(length(is_agg))
  out[is_agg] <- agg_chr
  out[!is_agg] <- format(x[["x"]][!is_agg], ...)
  out
}

#' @export
print.agg_vec <- function(x, ...) {
  cat(sprintf("<agg_vec[%d]>\n", length(x)))
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

# Not exported directly -- registered dynamically for pillar via
# register_s3_method() in zzz.R (pillar is a Suggests, not an Imports).
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

# Not exported directly -- registered dynamically for pillar via
# register_s3_method() in zzz.R, mirroring pillar_shaft.agg_vec() above.
# Gives a tibble column of agg_vec an abbreviated type header, e.g.
# "chr*", the same shape as vctrs' vec_ptype_abbr() used to produce.
type_sum.agg_vec <- function(x, ...) {
  paste0(pillar::type_sum(x[["x"]]), "*")
}

#' @export
length.agg_vec <- function(x) {
  length(x[["x"]])
}

#' @export
`[.agg_vec` <- function(x, i, ...) {
  new_agg_vec(x[["x"]][i], x[["agg"]][i])
}

#' @export
c.agg_vec <- function(...) {
  xs <- list(...)
  new_agg_vec(
    x = do.call(c, lapply(xs, function(x) x[["x"]])),
    agg = do.call(c, lapply(xs, function(x) x[["agg"]]))
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

  x1 <- e1[["x"]]
  x2 <- e2[["x"]]
  val_eq <- (x1 == x2) | (is.na(x1) & is.na(x2))
  val_eq[is.na(val_eq)] <- FALSE
  (e1[["agg"]] & e2[["agg"]]) | val_eq
}

#' @export
`!=.agg_vec` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
is.na.agg_vec <- function(x) {
  is.na(x[["x"]]) & !x[["agg"]]
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
    x[["agg"]]
  }
}
