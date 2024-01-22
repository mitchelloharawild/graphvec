#' Identify the children of a node in a graph
#'
#' @param x A graph vector
#' @param node The root node from which children are identified
#'
#' @importFrom igraph graph_from_edgelist subcomponent
#' @export
node_is_child <- function(x, node, ...){
  stopifnot(inherits(x, "graph_vec"))
  g <- attr(x, "g")
  g <- graph_from_edgelist(
    cbind(from = unlist(g[["from"]]), to = rep(g[["to"]], lengths(g[["from"]])))
  )
  unclass(x) %in% subcomponent(g, match(node, levels(x)), mode = "in")
}

#' Identify the degree of a node in a graph
#'
#' @param x A graph vector
#'
#' @export
node_degree <- function(x, ...){
  stopifnot(inherits(x, "graph_vec"))
  g <- attr(x, "g")
  g <- graph_from_edgelist(
    cbind(from = unlist(g[["from"]]), to = rep(g[["to"]], lengths(g[["from"]])))
  )
  igraph::degree(g)[vec_data(x)]
}
