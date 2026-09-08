# A small directed graph shared by several tests below:
# A -> B, A -> C, B -> D, C -> D, D -> E
chain <- function() {
  node_vec(
    x = c("A", "B", "C", "D", "E"),
    from = c(1L, 1L, 2L, 3L, 4L),
    to   = c(2L, 3L, 4L, 4L, 5L)
  )
}

test_that("node_degree() counts in/out/all correctly", {
  g <- chain()
  expect_equal(node_degree(g), c(2L, 2L, 2L, 3L, 1L))
  expect_equal(node_degree(g, mode = "out"), c(2L, 1L, 1L, 1L, 0L))
  expect_equal(node_degree(g, mode = "in"), c(0L, 1L, 1L, 2L, 1L))
})

test_that("node_degree() ignores mode for an undirected graph", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L), directed = FALSE)
  expect_equal(node_degree(g, mode = "all"), c(1L, 2L, 1L))
  expect_equal(node_degree(g, mode = "out"), node_degree(g, mode = "all"))
  expect_equal(node_degree(g, mode = "in"), node_degree(g, mode = "all"))
})

test_that("node_degree() counts an undirected self-loop once", {
  # The backend's convention (petgraph's neighbors_undirected()): one entry
  # per incident edge, so degree == length(neighbors). igraph's textbook
  # convention would say 2 here.
  g <- node_vec(x = c("A", "B"), from = 1L, to = 1L, directed = FALSE)
  expect_equal(node_degree(g), c(1L, 0L))
})

test_that("node_degree() counts a directed self-loop once per direction", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 1L)
  expect_equal(node_degree(g, mode = "out"), c(1L, 0L))
  expect_equal(node_degree(g, mode = "in"), c(1L, 0L))
  expect_equal(node_degree(g, mode = "all"), c(2L, 0L))
})

test_that("node_degree() returns a zero-length integer for a graph with no nodes", {
  g <- node_vec(x = character(), from = integer(), to = integer())
  expect_equal(node_degree(g), integer())
})

test_that("n_nodes()/n_edges() report the topology size", {
  g <- chain()
  expect_equal(n_nodes(g), 5L)
  expect_equal(n_edges(g), 5L)
  expect_equal(n_nodes(edges(g)), 5L)
  expect_equal(n_edges(edges(g)), 5L)
})

test_that("n_edges() counts a sliced edge_vec's own edges, not its parent graph's", {
  e <- edges(chain())[1:2]
  expect_equal(n_edges(e), 2L)
  expect_equal(n_edges(e), length(e))
  # The node space is unaffected by slicing edges away.
  expect_equal(n_nodes(e), 5L)
})

test_that("node-aligned measures see only a sliced edge_vec's own edges", {
  e <- edges(chain())[1:2] # A -> B, A -> C only
  expect_equal(node_degree(e), c(2L, 1L, 1L, 0L, 0L))
  expect_equal(node_degree(e, mode = "out"), c(2L, 0L, 0L, 0L, 0L))
})

test_that("graph_density() matches directed and undirected formulas", {
  d <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  expect_equal(graph_density(d), 2 / (3 * 2))

  u <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L), directed = FALSE)
  expect_equal(graph_density(u), 2 / 3)

  expect_equal(graph_density(node_vec(x = "A")), NaN)
  expect_equal(graph_density(node_vec(x = character())), NaN)
})

test_that("graph_density() counts the edges the object holds", {
  g <- chain()
  expect_equal(graph_density(g), 5 / (5 * 4))
  expect_equal(graph_density(edges(g)[1]), 1 / (5 * 4))
})

test_that("measures accept either orientation transparently", {
  g <- chain()
  e <- edges(g)
  expect_equal(node_degree(g), node_degree(e))
  expect_equal(n_nodes(g), n_nodes(e))
  expect_equal(n_edges(g), n_edges(e))
  expect_equal(graph_density(g), graph_density(e))
})

test_that("measures reject a non-graph input and hyperedges", {
  expect_error(node_degree(1:3), "node_vec")
  expect_error(n_nodes("not a graph"), "node_vec")

  h <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_error(node_degree(h), "hyperedges")
  expect_error(n_nodes(h), "hyperedges")
  expect_error(n_edges(h), "hyperedges")
  expect_error(graph_density(h), "hyperedges")
})
