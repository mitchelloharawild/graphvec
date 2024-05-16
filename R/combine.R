combine_graph <- function(x) {
  # Convert agg_vec to list(parent, child) graph structure
  list_graph <- function(x) {
    loc <- vec_group_loc(x)
    if (inherits(loc$key, "agg_vec")) {
      is_agg <- is_aggregated(loc$key)
      list(unlist(loc$loc[which(is_agg)], recursive = FALSE), loc$loc[!is_agg])
    } else if (inherits(loc$key, "graph_vec")) {
      g <- attr(loc$key, "g")
      # Match graph relationships to index positions
      g_i <- match(levels(x), loc$key)
      g[["to"]] <- lapply(g[["to"]], function(i) loc$loc[[g_i[i]]])
      g[["from"]] <- lapply(g[["from"]], function(x) lapply(x, function(i) loc$loc[[g_i[[i]]]]))

      # Invert from,to into a list of edges
      purrr::transpose(unname(g[c("to", "from")]))
    } else {
      # Vector without graph structure has no children
      c(loc$loc, list(list()))
    }
  }

  # Intersect
  graph_intersect <- function(x, y) {
    traverse_list(y, .f = function(.x, .y) compact(.x), .h = function(y) intersect(x, y))
  }

  # Convert list(parent, child) to edges matrix from,to,id
  list_to_edges <- function(x) {
    edges <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("from", "to", "id")))
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
    vctrs::new_data_frame(edges)
  }

  list_to_graph <- function(x) {
    e_env <- as.environment(list(e = list()))
    traverse_list(x, .f = function(x, y) {
      i <- vapply(x, is.integer, logical(1L))
      if(any(i) && any(!i)) {
        e_env$e[[length(e_env$e) + 1L]] <- list(x[[which(i)]], unlist(x[!i]))
        x <- x[which(i)]
      }
      x
    })
    x <- setNames(transpose(e_env$e), c("to", "from"))
    x[["to"]] <- unlist(x[["to"]])
    vctrs::new_data_frame(x)
  }

  flatten <- function(x) {
    if(length(x) == 1L && is.list(x[[1]])) flatten(x[[1L]]) else x
  }

  g <- Reduce(
    function(g, k) {
      int_g <- traverse_list(g, .f = function(x, y) flatten(x), .h = function(x) graph_intersect(x, k))
      int_k <- traverse_list(k, .f = function(x, y) flatten(x), .h = function(x) graph_intersect(x, g))

      c(int_g, int_k)
    }, lapply(x, list_graph)
  )

  vctrs::vec_unique(list_to_graph(g))
}
