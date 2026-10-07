# A -> B, A -> C, B -> D, C -> D, D -> E
chain <- function() {
  node_vec(
    x = c("A", "B", "C", "D", "E"),
    from = c(1L, 1L, 2L, 3L, 4L),
    to   = c(2L, 3L, 4L, 4L, 5L)
  )
}

test_that("node_is_isolated()/node_is_root()/node_is_leaf() identify boundary nodes", {
  g <- chain()
  expect_equal(node_is_isolated(g), rep(FALSE, 5))
  expect_equal(node_is_root(g), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(node_is_leaf(g), c(FALSE, FALSE, FALSE, FALSE, TRUE))

  iso <- node_vec(x = c("A", "B"), from = integer(), to = integer())
  expect_equal(node_is_isolated(iso), c(TRUE, TRUE))
  expect_equal(node_is_root(iso), c(TRUE, TRUE))
  expect_equal(node_is_leaf(iso), c(TRUE, TRUE))
})

test_that("node_is_root()/node_is_leaf() collapse onto isolation when undirected", {
  g <- node_vec(
    x = c("A", "B", "C"),
    from = 1L, to = 2L,
    directed = FALSE
  )
  expect_equal(node_is_root(g), node_is_isolated(g))
  expect_equal(node_is_leaf(g), node_is_isolated(g))
  expect_equal(node_is_isolated(g), c(FALSE, FALSE, TRUE))
})

test_that("a self-loop makes its node non-isolated", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 1L)
  expect_equal(node_is_isolated(g), c(FALSE, TRUE))
  expect_equal(node_is_root(g), c(FALSE, TRUE))
  expect_equal(node_is_leaf(g), c(FALSE, TRUE))

  # Still true under the "a self-loop counts once" undirected convention.
  u <- node_vec(x = c("A", "B"), from = 1L, to = 1L, directed = FALSE)
  expect_equal(node_is_isolated(u), c(FALSE, TRUE))
})

test_that("edge_is_loop()/edge_multiplicity()/edge_is_multi() flag self-loops and parallels", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 1L, 1L), to = c(1L, 2L, 2L))
  expect_equal(edge_is_loop(g), c(TRUE, FALSE, FALSE))
  expect_equal(edge_multiplicity(g), c(1L, 2L, 2L))
  expect_equal(edge_is_multi(g), c(FALSE, TRUE, TRUE))
})

test_that("edge_multiplicity() treats an undirected pair symmetrically", {
  g <- node_vec(x = c("A", "B"), from = c(1L, 2L), to = c(2L, 1L), directed = FALSE)
  expect_equal(edge_multiplicity(g), c(2L, 2L))

  # The same pair is two distinct ordered edges when directed.
  d <- node_vec(x = c("A", "B"), from = c(1L, 2L), to = c(2L, 1L))
  expect_equal(edge_multiplicity(d), c(1L, 1L))
})

test_that("edge predicates handle a graph with no edges", {
  g <- node_vec(x = c("A", "B"), from = integer(), to = integer())
  expect_equal(edge_is_loop(g), logical())
  expect_equal(edge_multiplicity(g), integer())
  expect_equal(edge_is_multi(g), logical())
  expect_false(graph_has_loops(g))
})

test_that("edge predicates are counted over a sliced edge_vec's own edges", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 1L, 1L), to = c(1L, 2L, 2L))
  e <- edges(g)[2] # one of the two parallel A -> B edges

  expect_equal(edge_is_loop(e), FALSE)
  expect_equal(edge_multiplicity(e), 1L)
  expect_equal(edge_is_multi(e), FALSE)
  expect_false(graph_has_loops(e))

  # ... and the loop is still found when it is the edge kept.
  expect_true(graph_has_loops(edges(g)[1]))
})

test_that("edge predicates follow a reordered edge selection", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 1L, 1L), to = c(1L, 2L, 2L))
  e <- edges(g)[c(3L, 1L, 2L)]
  expect_equal(edge_is_loop(e), c(FALSE, TRUE, FALSE))
  expect_equal(edge_multiplicity(e), c(2L, 1L, 2L))
})

test_that("graph_has_loops()/graph_is_directed() summarise the graph", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 1L)
  expect_true(graph_has_loops(g))
  expect_true(graph_is_directed(g))

  u <- node_vec(x = c("A", "B"), from = 1L, to = 2L, directed = FALSE)
  expect_false(graph_has_loops(u))
  expect_false(graph_is_directed(u))
})

test_that("predicates accept either orientation transparently", {
  g <- chain()
  e <- edges(g)
  expect_equal(node_is_isolated(g), node_is_isolated(e))
  expect_equal(node_is_root(g), node_is_root(e))
  expect_equal(node_is_leaf(g), node_is_leaf(e))
  expect_equal(edge_is_loop(g), edge_is_loop(e))
  expect_equal(edge_multiplicity(g), edge_multiplicity(e))
  expect_equal(graph_is_directed(g), graph_is_directed(e))
  expect_equal(graph_has_loops(g), graph_has_loops(e))
})

test_that("predicates reject a non-graph input and hyperedges", {
  expect_error(node_is_isolated(1:3), "node_vec")
  expect_error(graph_is_directed("not a graph"), "node_vec")

  h <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_error(node_is_isolated(h), "hyperedges")
  expect_error(edge_is_loop(h), "hyperedges")
  expect_error(edge_multiplicity(h), "hyperedges")
  expect_error(graph_is_directed(h), "hyperedges")
  expect_error(graph_has_loops(h), "hyperedges")
})

test_that("edge_multiplicity() gives NA for a missing edge and doesn't group missing edges", {
  ev <- edges(node_vec(c("a", "b", "c"), c(1L, 2L), c(2L, 3L)))
  e <- c(ev, ev[1], vctrs::vec_init(ev, 2))
  expect_equal(edge_multiplicity(e), c(2L, 1L, 2L, NA, NA))
  expect_equal(edge_is_multi(e), c(TRUE, FALSE, TRUE, NA, NA))
  expect_equal(edge_multiplicity(vctrs::vec_init(ev, 2)), c(NA_integer_, NA_integer_))
  # n_edges() still counts missing edges as elements; degree ignores them.
  expect_equal(n_edges(e), 5L)
  expect_equal(sum(node_degree(e)), 6L)
})
