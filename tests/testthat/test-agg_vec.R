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
