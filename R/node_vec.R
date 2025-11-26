#' Graph vector along nodes
#' 
#' A `node_vec` is a vector of graph nodes with associated edges stored as
#' attributes.
#' 
#' @param x A vector representing the nodes in the graph.
#' @param edges A data frame with columns `from` and `to` representing the edges
#' 
#' @examples
#' 
#' g <- node_vec(
#'  x = factor(c("A", "B", "C")),
#'  edges = data.frame(
#'   from = list(c(1L), c(2L), c(1L, 2L)),
#'   to = c(2L, 3L, 1L)
#'  )
#' )
#' 
#' 
#' @export
node_vec <- function(x = list(), edges = data.frame(from = list(), to = integer())) {
  # Check inputs
  stopifnot(vctrs::obj_is_vector(x))
  stopifnot(is.list(edges))

  # Validate edges
  edges[["from"]] <- vec_cast(edges[["from"]], list_of(.ptype = integer()))
  vctrs::vec_assert(edges[["to"]], integer())

  new_node_vec(x = x, edges = edges)
}

#' Constructor function for node_vec
#' 
#' @param x A vector representing the nodes in the graph.
#' @param edges A data frame with columns `from` and `to` representing the edges
#' 
#' @export
new_node_vec <- function(x = list(), edges = data.frame(from = list(), to = integer())) {
  out <- vctrs::new_vctr(x, edges = edges, class = "node_vec")

  # Restore attributes from x
  x_attr <- attributes(x)
  x_attr$class <- base::union(class(out), class(x))
  attributes(out)[names(x_attr)] <- x_attr
  out
}

#' @export
format.node_vec <- function(x, ...){
  class(x) <- class(x)[c(-1L, -2L)]
  format(x, ...)
}

#' @rdname aggregation-vctrs
#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 node_vec
#' @export
vec_ptype2.node_vec <- function(x, y, ...) UseMethod("vec_ptype2.node_vec", y)

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.node_vec.default <- function(x, y, ...) node_vec()

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.node_vec.character <- function(x, y, ...) node_vec()

#' @rdname aggregation-vctrs
#' @method vec_cast node_vec
#' @export
vec_cast.node_vec <- function(x, to, ...) UseMethod("vec_cast.node_vec")
#' @rdname aggregation-vctrs
#' @export
vec_cast.node_vec.node_vec <- function(x, to, ...) {
  # x <- vec_proxy(x)
  # if(all(x$agg)) x$x <- vec_rep(vec_cast(NA, vec_proxy(to)$x), length(x$x))
  vec_restore(x, to)
}
#' @rdname aggregation-vctrs
#' @export
vec_cast.node_vec.default <- function(x, to, ...) node_vec(x)
#' @rdname aggregation-vctrs
#' @export
vec_cast.character.node_vec <- function(x, to, ...) levels(x)[x]
#' @rdname aggregation-vctrs
#' @export
vec_cast.node_vec.character <- function(x, to, ...) {
  node_vec(factor(x, levels = levels(to)), g = attr(to, "g"))
}

#' @rdname aggregation-vctrs
#' @export
vec_ptype_abbr.node_vec <- function(x, ...) {
  "nodes"
  # class(x) <- class(x)[-1]
  # paste0(vec_ptype_abbr(x, ...), "'")
}
