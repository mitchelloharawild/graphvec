test_that("as.igraph() converts node_vec to an igraph object", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  ig <- igraph::as.igraph(g)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::ecount(ig), 2L)
})

test_that("as.igraph() converts agg_vec to an igraph object", {
  skip_if_not_installed("igraph")
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  ig <- igraph::as.igraph(v)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::vcount(ig), 3L)
  expect_equal(igraph::ecount(ig), 2L)
  expect_equal(igraph::as_edgelist(ig, names = FALSE), cbind(c(2L, 3L), c(1L, 1L)))
})

test_that("as.igraph() converts agg_vec as the graph nodes() gives it", {
  skip_if_not_installed("igraph")
  # Formerly a row-order "forest of stars", which disagreed with nodes().
  vs <- list(
    agg_vec(c(NA, "A", NA, "B"), aggregated = c(TRUE, FALSE, TRUE, FALSE)),
    agg_vec(c(NA, "A", "B", NA, "C"), aggregated = c(TRUE, FALSE, FALSE, TRUE, FALSE)),
    agg_vec(c("A", NA), aggregated = c(FALSE, TRUE)),
    agg_vec(c(NA, NA, "A", "B"), aggregated = c(TRUE, TRUE, FALSE, FALSE)),
    agg_vec(c("A", "B"), aggregated = c(FALSE, FALSE)),
    agg_vec()
  )
  for (v in vs) {
    expect_identical(
      igraph::as_edgelist(igraph::as.igraph(v), names = FALSE),
      igraph::as_edgelist(igraph::as.igraph(nodes(v)), names = FALSE)
    )
    expect_equal(igraph::vcount(igraph::as.igraph(v)), length(v))
  }
})

test_that("as.igraph() links every agg_vec row to the first `<aggregated>` row", {
  skip_if_not_installed("igraph")
  # Row order doesn't matter, and a later duplicate `<aggregated>` row (4) is
  # left isolated rather than starting a second star.
  v <- agg_vec(
    c(NA, "A", "B", NA, "C"),
    aggregated = c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  expect_equal(
    igraph::as_edgelist(igraph::as.igraph(v), names = FALSE),
    cbind(c(2L, 3L, 5L), c(1L, 1L, 1L))
  )

  v <- agg_vec(c("A", NA), aggregated = c(FALSE, TRUE))
  expect_equal(igraph::as_edgelist(igraph::as.igraph(v), names = FALSE), cbind(1L, 2L))

  # Consecutive `<aggregated>` rows are no longer a hyperedge error.
  v <- agg_vec(c(NA, NA, "A", "B"), aggregated = c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(
    igraph::as_edgelist(igraph::as.igraph(v), names = FALSE),
    cbind(c(3L, 4L), c(1L, 1L))
  )
})

test_that("as.igraph() errors on a hyperedge node_vec/edge_vec (igraph has no hyperedge concept)", {
  skip_if_not_installed("igraph")
  g <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_error(igraph::as.igraph(g), class = "rlang_error")

  e <- edge_vec(from = list(c(1L, 2L)), to = 3L, nodes = c("A", "B", "C"))
  expect_error(igraph::as.igraph(e), class = "rlang_error")
})

test_that("as.igraph() converts agg_df to an igraph object", {
  skip_if_not_installed("igraph")
  # A single crossed cell (row 1) with its two one-column aggregates (rows
  # 2-3) and the fully aggregated total (row 4): a diamond lattice.
  df <- agg_df(
    Purpose = agg_vec(c("Business", NA, "Business", NA), c(FALSE, TRUE, FALSE, TRUE)),
    State = agg_vec(c("NSW", "NSW", NA, NA), c(FALSE, FALSE, TRUE, TRUE))
  )
  ig <- igraph::as.igraph(df)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::vcount(ig), 4L)
  expect_equal(
    igraph::as_edgelist(ig, names = FALSE),
    cbind(c(1L, 3L, 1L, 2L), c(2L, 4L, 3L, 4L))
  )
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

test_that("as.igraph() respects node_vec's `directed` attribute", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  expect_true(igraph::is_directed(igraph::as.igraph(g)))

  gu <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L),
    directed = FALSE
  )
  expect_false(igraph::is_directed(igraph::as.igraph(gu)))
})

test_that("as.igraph() respects edge_vec's `directed` attribute", {
  skip_if_not_installed("igraph")
  e <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_true(igraph::is_directed(igraph::as.igraph(e)))

  eu <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C")),
    directed = FALSE
  )
  expect_false(igraph::is_directed(igraph::as.igraph(eu)))
})

test_that("as.igraph() counts every node of an edge_vec without node data", {
  skip_if_not_installed("igraph")
  e <- c(edge_vec(1:2, 2:3), edge_vec(1L, 2L))
  expect_equal(igraph::vcount(igraph::as.igraph(e)), 5L)
})
