test_that("nodes.node_vec() and edges.edge_vec() are the identity", {
  g <- node_vec(x = c("A", "B", "C"), from = 1L, to = 2L)
  expect_identical(nodes(g), g)

  e <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("A", "B")))
  expect_identical(edges(e), e)
})

test_that("edges(node_vec) converts to an edge_vec, preserving directed", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L),
    directed = FALSE
  )
  e <- edges(g)
  expect_s3_class(e, "edge_vec")
  expect_length(e, 3L)
  expect_false(attr(e, "directed"))
  expect_equal(format(e), c("[A]->[B]", "[B]->[C]", "[C]->[D]"))
})

test_that("nodes(edge_vec) converts to a node_vec, keeping isolated nodes", {
  e <- edge_vec(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nodes = c("A", "B", "C", "D"),
    directed = FALSE
  )
  n <- nodes(e)
  expect_s3_class(n, "node_vec")
  expect_length(n, 4L)
  expect_false(attr(n, "directed"))
  expect_equal(attr(n, "edges")$to, c(2L, 3L))
})

test_that("nodes(edge_vec) converts a data frame of node attributes losslessly", {
  e <- edge_vec(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nodes = data.frame(name = c("A", "B", "C", "D"), size = c(10, 4, 7, 1))
  )
  n <- nodes(e)
  expect_s3_class(n, "node_vec")
  expect_length(n, 4L)
  expect_equal(node_vec_data(n), data.frame(name = c("A", "B", "C", "D"), size = c(10, 4, 7, 1)))
})

test_that("nodes()/edges() round-trip losslessly in both directions", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L),
    directed = FALSE
  )
  g2 <- nodes(edges(g))
  expect_equal(format(g2), format(g))
  expect_equal(attr(g2, "edges"), attr(g, "edges"))
  expect_equal(attr(g2, "directed"), attr(g, "directed"))

  e <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), nodes = c("A", "B", "C"))
  e2 <- edges(nodes(e))
  expect_equal(format(e2), format(e))
  expect_equal(attr(e2, "directed"), attr(e, "directed"))

  # also round-trips when the node table is a data frame
  ed <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), nodes = data.frame(label = c("A", "B", "C")))
  ed2 <- edges(nodes(ed))
  expect_equal(format(ed2), format(ed))
})

test_that("edge attribute columns survive nodes()/edges() reorientation losslessly", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L),
    weight = c(1, 2, 5)
  )
  e <- edges(g)
  expect_equal(e$weight, c(1, 2, 5))

  g2 <- nodes(e)
  expect_equal(attr(g2, "edges")$weight, c(1, 2, 5))

  e2 <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), weight = c(10, 20), nodes = c("A", "B", "C"))
  n <- nodes(e2)
  expect_equal(attr(n, "edges")$weight, c(10, 20))
  e3 <- edges(n)
  expect_equal(e3$weight, c(10, 20))
})

test_that("node_vec() rejects hyperedges (from must be a plain integer vector)", {
  expect_error(node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L))
})
