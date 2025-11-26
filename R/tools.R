#' Identify the children of a node in a graph
#'
#' @param x A graph vector
#' @param node The root node from which children are identified
#'
#' @importFrom igraph graph_from_edgelist subcomponent
node_is_child <- function(x, node, ...){
  stopifnot(inherits(x, "node_vec"))
  g <- attr(x, "edges")
  g <- graph_from_edgelist(
    cbind(from = unlist(g[["from"]]), to = rep(g[["to"]], lengths(g[["from"]])))
  )

  unclass(x) %in% subcomponent(g, match(node, levels(x)), mode = "in")
}

#' Identify the degree of a node in a graph
#'
#' @param x A graph vector
node_degree <- function(x, ...){
  stopifnot(inherits(x, "node_vec"))
  g <- attr(x, "edges")
  g <- graph_from_edgelist(
    cbind(from = unlist(g[["from"]]), to = rep(g[["to"]], lengths(g[["from"]])))
  )
  igraph::degree(g)[vec_data(x)]
}


#' Identify the distance from a node in a graph
#'
#' @param x A graph vector
node_distance <- function(x, from, ...){
  stopifnot(inherits(x, "node_vec"))
  from <- match(from, levels(x))
  g <- attr(x, "edges")
  g <- graph_from_edgelist(
    cbind(from = unlist(g[["from"]]), to = rep(g[["to"]], lengths(g[["from"]])))
  )
  igraph::distances(g, from, ...)[vec_data(x)]
}

#' Identify disjoint graphs by node
#'
#' @param x A graph
node_disjoint_id <- function(x, ...) {
  igraph::components(graphvec_to_igraph(x))$membership
}
