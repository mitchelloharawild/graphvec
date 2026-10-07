#' The plain values of a node_vec
#'
#' `node_values()` gives the values a `node_vec` was built from (an atomic
#' vector or a data frame of node attributes), without the graph or the
#' nodes' identity. Nodes compare by identity (see [node_vec()]): `==`,
#' `match()`, `%in%` and joins only match nodes of the same graph, never a
#' plain value, so comparing nodes by their labels is done explicitly on
#' `node_values()` (or on `format()`).
#'
#' @param x A `node_vec`. Plain vectors and data frames, which are already
#'   values, are returned as they are.
#' @param ... Passed on to methods.
#'
#' @return The node values, one per node: a vector of the type the
#'   `node_vec` was built from, or a data frame.
#'
#' @examples
#' n <- node_vec(c("A", "B", "C", "A"), from = 1:3, to = 2:4)
#' node_values(n)
#'
#' # Match nodes by value, rather than by identity:
#' node_values(n) %in% c("A", "B")
#' node_values(n) == "A"
#'
#' @export
node_values <- function(x, ...) {
  UseMethod("node_values")
}

#' @export
node_values.node_vec <- function(x, ...) {
  node_vec_data(x)
}

# An edge_vec isn't one value per node, and an agg_vec's `<aggregated>`
# elements have no plain value, so neither has node values of its own.
#' @export
node_values.edge_vec <- function(x, ...) {
  cli::cli_abort(c(
    "{.fn node_values} needs a {.cls node_vec}, not an {.cls edge_vec}.",
    i = "Use {.code node_values(nodes(x))} for the values of its nodes, or {.code format(x)} for a label per edge."
  ))
}

#' @export
node_values.agg_vec <- function(x, ...) {
  cli::cli_abort(c(
    "{.fn node_values} needs a {.cls node_vec}, not an {.cls agg_vec}.",
    i = "Use {.code format(x)} or {.code as.character(x)} for a label per element."
  ))
}

#' @export
node_values.default <- function(x, ...) {
  x
}
