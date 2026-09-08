test_that("backend_of() reaches the graph from either orientation", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  expect_identical(backend_of(g), attr(g, "graph"))
  expect_identical(backend_of(edges(g)), attr(g, "graph"))
})

test_that("backend_of() rejects a hyperedge from/to column", {
  h <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_null(attr(h, "graph"))
  expect_error(backend_of(h), "hyperedges")
  expect_error(backend_of(edges(h)), "hyperedges")
})

test_that("backend_of() rejects a non-graph input", {
  expect_error(backend_of(1:3), "node_vec")
  expect_error(backend_of("not a graph"), "edge_vec")
})

test_that("op_graph() reuses the shared backend when nothing was sliced away", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  expect_identical(op_graph(g), attr(g, "graph"))
  # An unsliced edge_vec still spans every edge in the graph's own order.
  expect_identical(op_graph(edges(g)), attr(g, "graph"))
})

test_that("op_graph() rebuilds over exactly the edges a sliced edge_vec holds", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  e <- edges(g)[1]
  # The parent pointer is carried forward unchanged ...
  expect_identical(attr(e, "graph"), attr(g, "graph"))
  # ... but the graph the operations see covers only the selected edge.
  expect_equal(op_graph(e)$n_edges(), 1L)
  expect_equal(op_graph(e)$n_nodes(), 3L)
})

test_that("op_endpoints() is aligned to the object, not to the parent graph", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  expect_equal(op_endpoints(g), list(from = c(1L, 2L), to = c(2L, 3L)))

  e <- edges(g)[2]
  expect_equal(op_endpoints(e), list(from = 2L, to = 3L))

  # Reordering the edge selection reorders the endpoints with it.
  expect_equal(op_endpoints(edges(g)[2:1]), list(from = c(2L, 1L), to = c(3L, 2L)))
})

test_that("op_n_edges() counts the edges the object currently holds", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  expect_equal(op_n_edges(g), 2L)
  expect_equal(op_n_edges(edges(g)), 2L)
  expect_equal(op_n_edges(edges(g)[1]), 1L)
})

test_that("check_attr_length() errors on a length mismatch, never recycles", {
  expect_silent(check_attr_length(c(1, 2, 3), 3L))
  expect_error(check_attr_length(1, 3L), "length 3")
  expect_error(check_attr_length(1, 3L, arg = "values"), "values")
  # A recyclable length is still an error.
  expect_error(check_attr_length(c(1, 2), 4L))
})

test_that("check_weights_length() passes NULL and errors on a length mismatch", {
  expect_null(check_weights_length(NULL, 3L))
  expect_null(check_weights_length(c(1, 2, 3), 3L))
  expect_error(check_weights_length(c(1, 2), 3L), "never recycled")
})

test_that("query_selection() dispatches on length(i)", {
  # A scalar `i` returns the bare selection ...
  expect_equal(query_selection(2, function(pos) pos * 10L), 20L)
  # ... a vector `i` returns one element per query.
  expect_equal(
    query_selection(1:3, function(pos) seq_len(pos)),
    list(1L, 1:2, 1:3)
  )
  expect_type(query_selection(1:2, function(pos) pos), "list")
  # Length-1 stays bare even when the result itself is longer than 1.
  expect_equal(query_selection(3, function(pos) seq_len(pos)), 1:3)
})
