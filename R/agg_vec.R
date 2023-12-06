#' Create an aggregation vector
#'
#' \lifecycle{maturing}
#'
#' An aggregation vector extends usual vectors by adding `<aggregated>` values.
#' These vectors are typically produced via the [`aggregate_key()`] function,
#' however it can be useful to create them manually to produce more complicated
#' hierarchies (such as unbalanced hierarchies).
#'
#' @param x The vector of values.
#' @param aggregated A logical vector to identify which values are `<aggregated>`.
#'
#' @examples
#' agg_vec(
#'   x = c(NA, "A", "B"),
#'   aggregated = c(TRUE, FALSE, FALSE)
#' )
#'
#' @export
agg_vec <- function(x = character(), aggregated = logical(vec_size(x))){
  is_agg <- is_aggregated(x)
  if (inherits(x, "agg_vec")) x <- field(x, "x")
  x[is_agg] <- NA
  vec_assert(aggregated, ptype = logical())
  vctrs::new_rcrd(list(x = x, agg = is_agg | aggregated), class = "agg_vec")
}

#' @export
format.agg_vec <- function(x, ..., agg_chr = "<aggregated>"){
  n <- vec_size(x)
  x <- vec_data(x)
  is_agg <- x[["agg"]]
  out <- character(length = n)
  out[is_agg] <- agg_chr
  out[!is_agg] <- format(x[["x"]][!is_agg], ...)
  out
}

pillar_shaft.agg_vec <- function(x, ...) {
  if(requireNamespace("crayon")){
    agg_chr <- crayon::style("<aggregated>", crayon::make_style("#999999", grey = TRUE))
  }
  else{
    agg_chr <- "<aggregated>"
  }

  out <- format(x, agg_chr = agg_chr)

  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 10)
}

#' Internal vctrs methods
#'
#' These methods are the extensions that allow aggregation vectors to work with
#' vctrs.
#'
#' @keywords internal
#' @name aggregation-vctrs
NULL

#' @rdname aggregation-vctrs
#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 agg_vec
#' @export
vec_ptype2.agg_vec <- function(x, y, ...) UseMethod("vec_ptype2.agg_vec", y)
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec.agg_vec <- function(x, y, ...) {
  x <- vec_data(x)[["x"]]
  y <- vec_data(y)[["x"]]
  ptype <- if(!is_logical(x) && !is_logical(y)) {
    vec_ptype2(x, y)
  } else if (is_logical(x)) {
    y
  } else {
    x
  }
  agg_vec(ptype)
}
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec.default <- function(x, y, ...) agg_vec()
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec.character <- function(x, y, ...) agg_vec()
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.character.agg_vec <- function(x, y, ...) agg_vec()

#' @rdname aggregation-vctrs
#' @export
vec_ptype_abbr.agg_vec <- function(x, ...) {
  paste0(vctrs::vec_ptype_abbr(vec_data(x)[["x"]], ...), "*")
}

#' @rdname aggregation-vctrs
#' @method vec_cast agg_vec
#' @export
vec_cast.agg_vec <- function(x, to, ...) UseMethod("vec_cast.agg_vec")
#' @rdname aggregation-vctrs
#' @export
vec_cast.agg_vec.agg_vec <- function(x, to, ...) {
  x <- vec_proxy(x)
  if(all(x$agg)) x$x <- vec_rep(vec_cast(NA, vec_proxy(to)$x), length(x$x))
  vec_restore(x, to)
}
#' @rdname aggregation-vctrs
#' @export
vec_cast.agg_vec.default <- function(x, to, ...) agg_vec(x)
#' @export
vec_cast.agg_vec.character <- function(x, to, ...) agg_vec(x)
#' @rdname aggregation-vctrs
#' @export
vec_cast.character.agg_vec <- function(x, to, ...) trimws(format(x))

#' @rdname aggregation-vctrs
#' @export
vec_proxy_compare.agg_vec <- function(x, ...) {
  vec_proxy(x)[c(2,1)]
}

#' @export
`==.agg_vec` <- function(e1, e2){
  e1_agg <- inherits(e1, "agg_vec")
  e2_agg <- inherits(e2, "agg_vec")

  if(!e1_agg || !e2_agg){
    x <- list(e1,e2)[[which(!c(e1_agg, e2_agg))]]
    is_agg <- x == "<aggregated>"
    if(any(is_agg)){
      warn("<aggregated> character values have been converted to aggregated values.
Hint: If you're trying to compare aggregated values, use `is_aggregated()`.")
    }
    x <- agg_vec(ifelse(is_agg, NA, x), aggregated = is_agg)
    if(!e1_agg) e1 <- x else e2 <- x
  }

  x <- vec_recycle_common(e1, e2)
  e1 <- vec_proxy(x[[1]])
  e2 <- vec_proxy(x[[2]])
  out <- logical(vec_size(e1))
  (e1$agg & e2$agg) | vec_equal(e1$x, e2$x, na_equal = TRUE)
}

#' @export
`!=.agg_vec` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
is.na.agg_vec <- function(x) {
  is.na(field(x, "x")) & !field(x, "agg")
}

#' @importFrom dplyr recode
#' @export
recode.agg_vec <- function(.x, ...) {
  field(.x, "x") <- recode(field(.x, "x"), ...)
  .x
}

#' Is the element an aggregation of smaller data
#'
#' @param x An object.
#'
#' @seealso [`aggregate_key`]
#'
#' @export
is_aggregated <- function(x){
  if(!inherits(x, "agg_vec")){
    logical(vec_size(x))
  } else {
    vec_proxy(x)[["agg"]]
  }
}
