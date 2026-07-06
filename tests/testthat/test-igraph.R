test_that("as.igraph() converts node_vec to an igraph object", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    x = c("A", "B", "C"),
    edges = data.frame(from = I(list(1L, 2L)), to = c(2L, 3L))
  )
  ig <- igraph::as.igraph(g)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::ecount(ig), 2L)
})

test_that("as.igraph() converts agg_vec to an igraph object", {
  skip_if_not_installed("igraph")
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  attr(v, "edges") <- data.frame(from = I(list(1L)), to = 2L)
  ig <- igraph::as.igraph(v)
  expect_s3_class(ig, "igraph")
})

test_that("as.igraph() converts edge_vec to an igraph object", {
  skip_if_not_installed("igraph")
  e <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  ig <- igraph::as.igraph(e)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::ecount(ig), 4L)
})
