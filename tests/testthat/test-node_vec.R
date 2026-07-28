test_that("node_vec() returns a node_vec object", {
  g <- node_vec(
    x = c("A", "B", "C"),
    edges = data.frame(from = I(list(1L, 2L)), to = c(2L, 3L))
  )
  expect_s3_class(g, "node_vec")
})

test_that("node_vec() preserves the length of x", {
  g <- node_vec(
    x = factor(c("A", "B", "C", "D")),
    edges = data.frame(from = I(list(1L, 2L)), to = c(2L, 3L))
  )
  expect_length(g, 4L)
})

test_that("node_vec() stores edges as an attribute", {
  edges <- data.frame(from = I(list(1L, 2L)), to = c(2L, 3L))
  g <- node_vec(x = c("A", "B", "C"), edges = edges)
  stored <- attr(g, "edges")
  expect_equal(stored[["to"]], c(2L, 3L))
})

test_that("node_vec() accepts an empty vector", {
  g <- node_vec()
  expect_s3_class(g, "node_vec")
  expect_length(g, 0L)
})

test_that("format.node_vec() formats the underlying vector", {
  g <- node_vec(
    x = factor(c("A", "B")),
    edges = data.frame(from = I(list(1L)), to = 2L)
  )

  expect_equal(format(g), c("A", "B"))
})

test_that("format.node_vec() preserves wrapped vctrs vector formatting", {
  g <- node_vec(
    x = agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE)),
    edges = data.frame(from = I(list(1L)), to = 2L)
  )

  expect_equal(format(g), c("<aggregated>", "A", "B"))
})

test_that("new_node_vec() is a low-level constructor for node_vec", {
  g <- new_node_vec(
    x = c("A", "B"),
    edges = data.frame(from = I(list(1L)), to = 2L)
  )
  expect_s3_class(g, "node_vec")
})
