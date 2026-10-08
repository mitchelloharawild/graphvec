# The node and edge queries each make one call into the backend for every
# queried position at once (`degrees()`, `neighbors_many()`,
# `incident_many()`, `edge_endpoints(ids)`). These check them against a
# naive per-position reference computed straight from the edge list, on
# random graphs with self-loops and parallel edges, in every representation
# the backend picks.

# Degree of every node, from the edge list: a directed self-loop counts once
# per direction, an undirected one once.
ref_degree <- function(n, from, to, directed, mode) {
  out <- tabulate(from, n)
  inn <- tabulate(to, n)
  if (!directed) {
    return(out + inn - tabulate(from[from == to], n))
  }
  switch(mode, out = out, `in` = inn, all = out + inn)
}

# Sorted neighbours of node `v`, one per incident edge.
ref_neighbors <- function(v, from, to, directed, mode) {
  if (!directed) {
    return(sort(c(to[from == v], from[to == v & from != v])))
  }
  out <- if (mode != "in") to[from == v] else integer()
  inn <- if (mode != "out") from[to == v] else integer()
  sort(c(out, inn))
}

# Increasing ids of the edges incident to node `v`, as edge_incident() gave
# them when it scanned the edge list itself: an undirected self-loop once,
# a directed one once per direction. Missing (NA) edges never match.
ref_incident <- function(v, from, to, directed, mode) {
  if (!directed) {
    return(which(from == v | to == v))
  }
  out <- if (mode != "in") which(from == v) else integer()
  inn <- if (mode != "out") which(to == v) else integer()
  sort(c(out, inn))
}

# A random graph on `n` nodes with `m` edges, a few forced self-loops, and
# (when `multi`) a few forced parallel edges.
random_edges <- function(n, m, multi) {
  from <- sample.int(n, m, replace = TRUE)
  to <- sample.int(n, m, replace = TRUE)
  loops <- sample.int(m, 2L)
  to[loops] <- from[loops]
  if (multi) {
    dup <- sample.int(m, 2L)
    from <- c(from, from[dup])
    to <- c(to, to[dup])
  } else {
    # Keep only the first of any repeated (unordered, if undirected) pair,
    # so the graph can take the Dense/Csr representations.
    key <- paste(pmin(from, to), pmax(from, to))
    keep <- !duplicated(key)
    from <- from[keep]
    to <- to[keep]
  }
  list(from = from, to = to)
}

test_that("vectorised node queries match a per-node reference", {
  set.seed(20261008)
  # (n, m, multi): dense, sparse (csr), and multigraph (general) shapes.
  shapes <- list(c(6, 12, 0), c(40, 30, 0), c(40, 60, 1), c(25, 80, 1))
  reprs <- character()
  for (shape in shapes) {
    for (directed in c(TRUE, FALSE)) {
      n <- shape[[1]]
      ends <- random_edges(n, shape[[2]], as.logical(shape[[3]]))
      g <- node_vec(seq_len(n), ends$from, ends$to, directed = directed)
      reprs <- c(reprs, attr(g, "graph")$repr_name())
      # Repeated query positions, and a single one.
      i <- c(sample.int(n, n + 5L, replace = TRUE), 1L)
      for (mode in c("all", "out", "in")) {
        expect_identical(
          node_degree(g, mode),
          ref_degree(n, ends$from, ends$to, directed, mode)
        )
        expected <- lapply(i, ref_neighbors, ends$from, ends$to, directed, mode)
        expect_identical(node_neighbors(g, i, mode), expected)
        expect_identical(node_neighbors(g, i[[1L]], mode), expected[[1L]])
        expect_identical(lengths(expected), node_degree(g, mode)[i])

        expected <- lapply(i, ref_incident, ends$from, ends$to, directed, mode)
        expect_identical(edge_incident(g, i, mode), expected)
        expect_identical(edge_incident(g, i[[1L]], mode), expected[[1L]])
        expect_identical(lengths(expected), node_degree(g, mode)[i])
      }
      deg <- ref_degree(n, ends$from, ends$to, directed, "all")
      expect_identical(node_is_isolated(g), deg == 0L)
      expect_identical(
        node_is_root(g),
        ref_degree(n, ends$from, ends$to, directed, "in") == 0L
      )
      expect_identical(
        node_is_leaf(g),
        ref_degree(n, ends$from, ends$to, directed, "out") == 0L
      )

      m <- length(ends$from)
      k <- sample.int(m, 7L, replace = TRUE)
      expect_identical(
        node_incident(g, k),
        lapply(k, function(e) c(ends$from[e], ends$to[e]))
      )
    }
  }
  expect_setequal(reprs, c("dense", "csr", "general"))
})

test_that("a sliced edge_vec reads only its own edges, missing ones as NA", {
  set.seed(1008)
  n <- 30L
  ends <- random_edges(n, 50L, TRUE)
  for (directed in c(TRUE, FALSE)) {
    e <- edges(node_vec(seq_len(n), ends$from, ends$to, directed = directed))
    # Repeated edges, a missing edge, and a reordering.
    id <- c(sample.int(length(e), 20L, replace = TRUE), NA, 3L, 3L)
    x <- e[id]
    expect_identical(attr(x, "edge_id"), id)

    got <- edge_vec_endpoints(x)
    expect_identical(got, list(from = ends$from[id], to = ends$to[id]))
    expect_identical(edge_is_loop(x), ends$from[id] == ends$to[id])
    expect_identical(
      node_incident(x, seq_along(id)),
      lapply(id, function(e) c(ends$from[e], ends$to[e]))
    )

    # Node measures see only the edges the slice holds, not the missing one.
    kept <- id[!is.na(id)]
    for (mode in c("all", "out", "in")) {
      expect_identical(
        node_degree(x, mode),
        ref_degree(n, ends$from[kept], ends$to[kept], directed, mode)
      )
      expect_identical(
        node_neighbors(x, 1:n, mode),
        lapply(1:n, ref_neighbors, ends$from[kept], ends$to[kept], directed, mode)
      )
      # Edge positions are the slice's own, the missing edge never incident.
      expect_identical(
        edge_incident(x, 1:n, mode),
        lapply(1:n, ref_incident, ends$from[id], ends$to[id], directed, mode)
      )
    }
  }
})

test_that("vectorised queries handle empty graphs and empty queries", {
  g <- node_vec(c("a", "b"), integer(), integer())
  expect_identical(node_degree(g), c(0L, 0L))
  expect_identical(node_neighbors(g, 2L), integer())
  expect_identical(node_neighbors(g, integer()), list())
  expect_identical(node_neighbors(g, 1:2), list(integer(), integer()))
  expect_identical(node_incident(g, integer()), list())
  expect_identical(edge_incident(g, 2L), integer())
  expect_identical(edge_incident(g, integer()), list())
  expect_identical(node_degree(node_vec(character(), integer(), integer())), integer())
})
