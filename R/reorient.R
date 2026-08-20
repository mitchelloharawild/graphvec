#' Reorient a graph vector
#'
#' `nodes()` and `edges()` return the same underlying graph, enumerated along
#' the node or edge axis respectively, regardless of which orientation `x`
#' started in. Both directions are lossless and involutive: node attributes,
#' isolated nodes, and `directed` all survive, because both orientations
#' carry the same logical graph — reorientation only changes which table the
#' result is indexed by.
#'
#' @param x A `node_vec` or `edge_vec`.
#' @param ... Passed on to methods.
#'
#' @return `nodes()` returns a `node_vec`. `edges()` returns an `edge_vec`.
#'
#' @examples
#' g <- node_vec(
#'   x = c("A", "B", "C"),
#'   from = c(1L, 2L),
#'   to = c(2L, 3L)
#' )
#' edges(g)
#' nodes(edges(g))
#'
#' @rdname reorient
#' @export
nodes <- function(x, ...) {
  UseMethod("nodes")
}

#' @rdname reorient
#' @export
edges <- function(x, ...) {
  UseMethod("edges")
}
