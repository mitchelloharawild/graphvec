test_that("agg_vec() returns an agg_vec object", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_s3_class(v, "agg_vec")
})

test_that("agg_vec() preserves length", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_length(v, 3L)
})

test_that("is_aggregated() correctly identifies aggregated elements", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_equal(is_aggregated(v), c(TRUE, FALSE, FALSE))
})

test_that("is_aggregated() returns all-FALSE for non-agg_vec", {
  expect_equal(is_aggregated(c("A", "B", "C")), c(FALSE, FALSE, FALSE))
})

test_that("agg_vec() accepts an empty vector", {
  v <- agg_vec()
  expect_s3_class(v, "agg_vec")
  expect_length(v, 0L)
})

test_that("format.agg_vec() renders aggregated values as <aggregated>", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_equal(format(v)[1], "<aggregated>")
})

test_that("is.na.agg_vec() returns FALSE for aggregated elements", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  # Aggregated values are not considered NA
  expect_false(is.na(v)[1])
})

test_that("agg_vec() stores only disaggregated values in the primary vector", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_identical(unclass(v), structure(list(c("A", "B")), agg_pos = 1L))
})

test_that("`[.agg_vec` subsets by full-length position", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  expect_equal(format(v[2:3]), c("A", "B"))
  expect_equal(format(v[1]), "<aggregated>")
  expect_equal(format(v[c(1, 2, 1)]), c("<aggregated>", "A", "<aggregated>"))
})

test_that("c.agg_vec() offsets aggregated positions across inputs", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  v2 <- c(v[1], v[2:3])
  expect_equal(format(v2), format(v))
  expect_identical(unclass(v2), unclass(v))
})

test_that("agg_vec() preserves the class of factor-backed values", {
  v <- agg_vec(factor(c(NA, "A", "B"), levels = c("A", "B")), aggregated = c(TRUE, FALSE, FALSE))
  expect_s3_class(agg_vec_values(v), "factor")
  expect_equal(format(v), c("<aggregated>", "A", "B"))
})

test_that("agg_vec() re-wraps an existing agg_vec, merging aggregated flags", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  v2 <- agg_vec(v, aggregated = c(FALSE, FALSE, TRUE))
  expect_equal(is_aggregated(v2), c(TRUE, FALSE, TRUE))
  expect_equal(format(v2), c("<aggregated>", "A", "<aggregated>"))
})
