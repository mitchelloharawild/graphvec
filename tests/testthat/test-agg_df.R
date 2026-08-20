test_that("agg_df() returns an agg_df object", {
  d <- agg_df(x = agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE)))
  expect_s3_class(d, "agg_df")
})

test_that("agg_df() requires named, agg_vec-only columns of equal length", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_error(agg_df(v), "named")
  expect_error(agg_df(x = c("A", "B", "C")), "agg_vec")
  expect_error(agg_df(x = v, y = v[1:2]), "same length")
})

test_that("length.agg_df() returns the row count", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_length(agg_df(x = v), 3L)
  expect_length(agg_df(), 0L)
})

test_that("format.agg_df() pastes columns together", {
  x <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  y <- agg_vec(c("P", NA, "Q"), aggregated = c(FALSE, TRUE, FALSE))
  expect_equal(format(agg_df(x = x, y = y)), c("<aggregated>:P", "A:<aggregated>", "B:Q"))
})

test_that("`[.agg_df` subsets rows across all columns", {
  x <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  y <- agg_vec(c("P", NA, "Q"), aggregated = c(FALSE, TRUE, FALSE))
  d <- agg_df(x = x, y = y)
  expect_equal(format(d[2:3]), c("A:<aggregated>", "B:Q"))
})

test_that("c.agg_df() row-binds matching-column agg_df objects", {
  x <- agg_vec(c(NA, "A"), aggregated = c(TRUE, FALSE))
  y <- agg_vec(c("P", NA), aggregated = c(FALSE, TRUE))
  d <- agg_df(x = x, y = y)
  d2 <- c(d[1], d[2])
  expect_equal(format(d2), format(d))

  expect_error(c(d, agg_df(z = x)), "different columns")
})

test_that("nodes()/edges() on a single-column agg_vec form a bipartite star", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  e <- attr(nodes(v), "edges")
  expect_equal(e$from, c(2L, 3L))
  expect_equal(e$to, c(1L, 1L))
  expect_s3_class(edges(v), "edge_vec")
})

test_that("nodes()/edges() on an agg_df form the crossed aggregation lattice", {
  # Purpose * State: total, Purpose-only, State-only, and fully disaggregated rows.
  purpose <- agg_vec(
    c(NA, NA, NA, "Business", "Holiday", "Business", "Business", "Holiday", "Holiday"),
    aggregated = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  state <- agg_vec(
    c(NA, "NSW", "VIC", NA, NA, "NSW", "VIC", "NSW", "VIC"),
    aggregated = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  kd <- agg_df(Purpose = purpose, State = state)

  n <- nodes(kd)
  expect_s3_class(n, "node_vec")
  expect_length(n, 9L)

  e <- attr(n, "edges")
  expect_equal(nrow(e), 12L)

  # Each fully disaggregated row (6-9) has two parents: its Purpose total and
  # its State total.
  expected <- data.frame(
    from = c(4L, 5L, 6L, 7L, 8L, 9L, 2L, 3L, 6L, 7L, 8L, 9L),
    to =   c(1L, 1L, 2L, 3L, 2L, 3L, 1L, 1L, 4L, 4L, 5L, 5L)
  )
  observed <- e[order(e$from, e$to), ]
  rownames(observed) <- NULL
  expect_equal(observed, expected[order(expected$from, expected$to), ], ignore_attr = TRUE)
})
