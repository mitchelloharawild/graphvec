#' @export
graph_vec <- function(x = factor(), g = data.frame()) {
  # Check inputs
  stopifnot(inherits(x,"factor"))
  stopifnot(is.list(g))

  # Validate edges
  g[["from"]] <- vec_cast(g[["from"]], list_of(.ptype = integer()))
  vctrs::vec_assert(g[["to"]], integer())

  vctrs::new_vctr(x, levels = levels(x), g = g, class = c("graph_vec", "factor"))
}

#' @export
format.graph_vec <- function(x, ...){
  format(as.character(x), ...)
}

#' @export
levels.graph_vec <- function(x, ...) {
  attr(x, "levels")
}

#' @rdname aggregation-vctrs
#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 graph_vec
#' @export
vec_ptype2.graph_vec <- function(x, y, ...) UseMethod("vec_ptype2.graph_vec", y)

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.graph_vec.default <- function(x, y, ...) graph_vec()

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.graph_vec.character <- function(x, y, ...) graph_vec()

#' @rdname aggregation-vctrs
#' @method vec_cast graph_vec
#' @export
vec_cast.graph_vec <- function(x, to, ...) UseMethod("vec_cast.graph_vec")
#' @rdname aggregation-vctrs
#' @export
vec_cast.graph_vec.graph_vec <- function(x, to, ...) {
  x <- vec_proxy(x)
  if(all(x$agg)) x$x <- vec_rep(vec_cast(NA, vec_proxy(to)$x), length(x$x))
  vec_restore(x, to)
}
#' @rdname aggregation-vctrs
#' @export
vec_cast.graph_vec.default <- function(x, to, ...) graph_vec(x)
#' @rdname aggregation-vctrs
#' @export
vec_cast.character.graph_vec <- function(x, to, ...) levels(x)[x]
#' @rdname aggregation-vctrs
#' @export
vec_cast.graph_vec.character <- function(x, to, ...) {
  graph_vec(factor(x, levels = levels(to)), g = attr(to, "g"))
}

#' @rdname aggregation-vctrs
#' @export
vec_ptype_abbr.graph_vec <- function(x, ...) {
  "graph"
  # class(x) <- class(x)[-1]
  # paste0(vec_ptype_abbr(x, ...), "'")
}
