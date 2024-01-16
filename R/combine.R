combine_graph <- function(x) {
  # Convert agg_vec to list(parent, child) graph structure
  list_graph <- function(x) {
    loc <- vec_group_loc(x)
    if (inherits(loc$key, "agg_vec")) {
      is_agg <- is_aggregated(loc$key)
      list(unlist(loc$loc[which(is_agg)], recursive = FALSE), loc$loc[!is_agg])
    } else if (inherits(loc$key, "graph_vec")) {
      e <- attr(loc$key, "e")

      e_loc <- vec_group_loc(e[,c("to", "id")])
      g <- .mapply(
        function(parent, child) {
          list(
            loc$loc[[parent]],
            lapply(e[child,"from"], function(i) loc$loc[[i]])
          )
        },
        dots = list(
          e_loc$key[,"to"],
          e_loc$loc
        ),
        MoreArgs = list()
      )
      g
      # lobstr::tree(g)
    } else {
      list(loc$loc)
    }
  }

  # Intersect
  graph_intersect <- function(x, y) {
    traverse_list(y, .f = function(.x, .y) compact(.x), .h = function(y) intersect(x, y))
  }

  # Convert list(parent, child) to edges matrix from,to,id
  list_to_edges <- function(x) {
    edges <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("from", "to", "id")))
    # lobstr::tree(x)
    e_env <- as.environment(list(id = 0))
    traverse_list(x, .f = function(x, y) {
      i <- vapply(x, is.integer, logical(1L))
      # if(sum(i) > 1) return(unlist(x, recursive = FALSE))
      if(any(i) && any(!i)) {
        assign("id", e_env$id + 1, e_env)
        edges <<- rbind(edges, cbind(unlist(x[!i]), x[[which(i)]], e_env$id))
        x <- x[which(i)]
      }
      # if(all(i)) x <- unlist(x, recursive = FALSE)
      x
    })
    as_tibble(edges)
  }

  g <- Reduce(
    function(g, k) {
      int_g <- traverse_list(g, .f = function(x, y) x, .h = function(x) graph_intersect(x, k))
      int_k <- traverse_list(k, .f = function(x, y) x, .h = function(x) graph_intersect(x, g))

      c(int_g, int_k)
    }, lapply(x, list_graph)
  )

  list_to_edges(g)
}
