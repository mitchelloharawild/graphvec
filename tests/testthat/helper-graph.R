# A node_vec's edges as "from->to" position pairs.
edge_pairs <- function(x) {
  e <- as.data.frame(edges(x))
  paste0(e[["from"]], "->", e[["to"]])
}
