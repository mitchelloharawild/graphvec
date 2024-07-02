graphvec_to_igraph <- function(g) {
  # Temporary design hack to allow for direct passing of combined graph
  if(!all(c("from", "to") %in% names(g))) {
    if(inherits(g, c("agg_vec", "graph_vec"))) {
      g <- list(g)
    }
    g <- combine_graph(g)
  }

  graph_from_edgelist(
    cbind(from = unlist(g[["from"]]), to = rep(g[["to"]], lengths(g[["from"]])))
  )
}
