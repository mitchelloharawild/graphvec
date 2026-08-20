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

test_that("as.igraph() connects agg_vec's forest of stars, one per aggregate root", {
  skip_if_not_installed("igraph")
  # Two disjoint stars: row 1 aggregates rows 2-3, row 4 aggregates row 5.
  v <- agg_vec(
    c(NA, "A", "B", NA, "C"),
    aggregated = c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  ig <- igraph::as.igraph(v)
  expect_equal(igraph::vcount(ig), 5L)
  expect_equal(
    igraph::as_edgelist(ig, names = FALSE),
    cbind(c(2L, 3L, 5L), c(1L, 1L, 4L))
  )
})

test_that("as.igraph() drops agg_vec rows with no preceding aggregate root", {
  skip_if_not_installed("igraph")
  v <- agg_vec(c("A", NA), aggregated = c(FALSE, TRUE))
  ig <- igraph::as.igraph(v)
  expect_equal(igraph::vcount(ig), 2L)
  expect_equal(igraph::ecount(ig), 0L)
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
