# A -> B, A -> C, B -> D, C -> D, D -> E
chain <- function() {
  node_vec(
    x = c("A", "B", "C", "D", "E"),
    from = c(1L, 1L, 2L, 3L, 4L),
    to   = c(2L, 3L, 4L, 4L, 5L)
  )
}

test_that("node_neighbors() returns successors/predecessors/both", {
  g <- chain()
  expect_equal(node_neighbors(g, 1, mode = "out"), c(2L, 3L))
  expect_equal(node_neighbors(g, 4, mode = "in"), c(2L, 3L))
  expect_equal(node_neighbors(g, 4, mode = "all"), c(2L, 3L, 5L))
  expect_equal(node_neighbors(g, 5, mode = "out"), integer())
})

test_that("node_neighbors() ignores mode for an undirected graph", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L), directed = FALSE)
  expect_equal(node_neighbors(g, 2, mode = "out"), c(1L, 3L))
  expect_equal(node_neighbors(g, 2, mode = "in"), c(1L, 3L))
  expect_equal(node_neighbors(g, 2, mode = "all"), c(1L, 3L))
})

test_that("node_neighbors() returns one entry per incident edge, sorted", {
  # Two parallel A -> B edges: B is listed twice, not deduplicated.
  g <- node_vec(x = c("A", "B"), from = c(1L, 1L), to = c(2L, 2L))
  expect_equal(node_neighbors(g, 1, mode = "out"), c(2L, 2L))

  # Sorted, so the result doesn't depend on which physical representation
  # the backend picked for the graph.
  rev_g <- node_vec(x = c("A", "B", "C"), from = c(1L, 1L), to = c(3L, 2L))
  expect_equal(node_neighbors(rev_g, 1, mode = "out"), c(2L, 3L))
})

test_that("node_neighbors() vectorises over `i` into a list", {
  g <- chain()
  out <- node_neighbors(g, 1:2, mode = "out")
  expect_type(out, "list")
  expect_equal(out, list(c(2L, 3L), 4L))
})

test_that("node_parents()/node_children() alias node_neighbors()", {
  g <- chain()
  expect_equal(node_parents(g, 4), node_neighbors(g, 4, mode = "in"))
  expect_equal(node_children(g, 1), node_neighbors(g, 1, mode = "out"))
  expect_equal(node_children(g, 1:2), node_neighbors(g, 1:2, mode = "out"))
})

test_that("node_neighbors() counts an undirected self-loop once", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 1L, directed = FALSE)
  expect_equal(node_neighbors(g, 1), 1L)
})

test_that("node_neighbors() counts a directed self-loop once per direction", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 1L)
  expect_equal(node_neighbors(g, 1, mode = "out"), 1L)
  expect_equal(node_neighbors(g, 1, mode = "in"), 1L)
  expect_equal(node_neighbors(g, 1, mode = "all"), c(1L, 1L))
})

test_that("edge_incident() returns incident edge positions", {
  g <- chain()
  expect_equal(edge_incident(g, 4, mode = "in"), c(3L, 4L))
  expect_equal(edge_incident(g, 4, mode = "out"), 5L)
  expect_equal(edge_incident(g, 4, mode = "all"), c(3L, 4L, 5L))
  expect_equal(edge_incident(g, 1, mode = "in"), integer())
})

test_that("edge_incident() lists an undirected edge once, whichever role", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L), directed = FALSE)
  expect_equal(edge_incident(g, 2, mode = "out"), c(1L, 2L))
  expect_equal(edge_incident(g, 2, mode = "in"), c(1L, 2L))
  # An undirected self-loop is one edge and is listed once.
  loop <- node_vec(x = c("A", "B"), from = 1L, to = 1L, directed = FALSE)
  expect_equal(edge_incident(loop, 1), 1L)
})

test_that("edge_incident() vectorises over `i` into a list", {
  g <- chain()
  expect_equal(edge_incident(g, 1:2, mode = "out"), list(c(1L, 2L), 3L))
})

test_that("node_degree() agrees with node_neighbors() and edge_incident()", {
  # A self-loop, a parallel pair, and a back edge in one graph.
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 1L, 1L, 2L, 3L),
    to   = c(1L, 2L, 2L, 3L, 1L)
  )
  for (mode in c("all", "out", "in")) {
    expect_equal(
      node_degree(g, mode = mode),
      lengths(node_neighbors(g, 1:3, mode = mode)),
      info = mode
    )
    expect_equal(
      node_degree(g, mode = mode),
      lengths(edge_incident(g, 1:3, mode = mode)),
      info = mode
    )
  }

  u <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 1L, 1L, 2L),
    to   = c(1L, 2L, 2L, 3L),
    directed = FALSE
  )
  expect_equal(node_degree(u), lengths(node_neighbors(u, 1:3)))
  expect_equal(node_degree(u), lengths(edge_incident(u, 1:3)))
})

test_that("node_incident() returns an edge's endpoints", {
  g <- chain()
  expect_equal(node_incident(g, 1), c(1L, 2L))
  expect_equal(node_incident(g, 1:2), list(c(1L, 2L), c(1L, 3L)))

  # A self-loop returns the same position twice.
  loop <- node_vec(x = c("A", "B"), from = 1L, to = 1L)
  expect_equal(node_incident(loop, 1), c(1L, 1L))
})

test_that("edge_heads()/edge_tails() match node_vec's own $to/$from", {
  g <- chain()
  e <- edges(g)
  expect_equal(edge_heads(g), e$to)
  expect_equal(edge_tails(g), e$from)
  expect_equal(edge_heads(e), e$to)
  expect_equal(edge_tails(e), e$from)
  expect_length(edge_heads(g), n_edges(g))
})

test_that("edge-aligned operations follow a sliced edge_vec's own edges", {
  g <- chain()
  e <- edges(g)[c(4L, 1L)] # C -> D, then A -> B

  expect_equal(edge_tails(e), c("C", "A"))
  expect_equal(edge_heads(e), c("D", "B"))
  expect_equal(node_incident(e, 1), c(3L, 4L))
  expect_equal(node_incident(e, 1:2), list(c(3L, 4L), c(1L, 2L)))
  # Edge positions come back as positions in `e`, not in the parent graph.
  expect_equal(edge_incident(e, 1, mode = "out"), 2L)
  expect_equal(node_neighbors(e, 1, mode = "out"), 2L)
})

test_that("traversal operations accept either orientation transparently", {
  g <- chain()
  e <- edges(g)
  expect_equal(node_neighbors(g, 1), node_neighbors(e, 1))
  expect_equal(node_parents(g, 4), node_parents(e, 4))
  expect_equal(edge_incident(g, 4, mode = "all"), edge_incident(e, 4, mode = "all"))
  expect_equal(node_incident(g, 1), node_incident(e, 1))
})

test_that("traversal operations reject a non-graph input and hyperedges", {
  expect_error(node_neighbors(1:3, 1), "node_vec")

  h <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_error(node_neighbors(h, 1), "hyperedges")
  expect_error(edge_incident(h, 1), "hyperedges")
  expect_error(node_incident(h, 1), "hyperedges")
  expect_error(edge_heads(h), "hyperedges")
  expect_error(edge_tails(h), "hyperedges")
})
