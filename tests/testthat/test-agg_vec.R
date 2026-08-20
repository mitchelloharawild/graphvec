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

test_that("`==.agg_vec` treats two aggregated positions as equal", {
  va <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  vb <- agg_vec(c(NA, "A", "C"), aggregated = c(TRUE, FALSE, FALSE))
  expect_equal(va == vb, c(TRUE, TRUE, FALSE))
})

test_that("`==.agg_vec` treats an aggregated position and a disaggregated position as unequal", {
  va <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  vc <- agg_vec(c("X", "A", "B"), aggregated = c(FALSE, FALSE, FALSE))
  expect_equal(va == vc, c(FALSE, TRUE, TRUE))
})

test_that("`==.agg_vec` compares disaggregated values normally, including matching NAs", {
  vd <- agg_vec(c(NA_character_, "A", "B"), aggregated = c(FALSE, FALSE, FALSE))
  ve <- agg_vec(c(NA_character_, "A", "C"), aggregated = c(FALSE, FALSE, FALSE))
  expect_equal(vd == ve, c(TRUE, TRUE, FALSE))
})

test_that("`==.agg_vec` treats a mismatched NA (one side missing, other not) as unequal", {
  vd <- agg_vec(c(NA_character_, "A"), aggregated = c(FALSE, FALSE))
  vf <- agg_vec(c("X", "A"), aggregated = c(FALSE, FALSE))
  expect_equal(vd == vf, c(FALSE, TRUE))
})

test_that("`==.agg_vec` compares against a plain vector as fully disaggregated, without string-matching \"<aggregated>\"", {
  va <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  plain <- c("<aggregated>", "A", "B")

  expect_no_warning(result <- va == plain)
  # The aggregated position in `va` does not match the literal text "<aggregated>".
  expect_equal(result, c(FALSE, TRUE, TRUE))

  # Comparing a plain "<aggregated>" string against an actual disaggregated
  # "<aggregated>" value is an ordinary (matching) string comparison.
  vg <- agg_vec("<aggregated>", aggregated = FALSE)
  expect_no_warning(expect_true(vg == "<aggregated>"))
})

test_that("`!=.agg_vec` is the negation of `==.agg_vec`", {
  va <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  vb <- agg_vec(c(NA, "A", "C"), aggregated = c(TRUE, FALSE, FALSE))
  expect_equal(va != vb, !(va == vb))
  expect_equal(va != vb, c(FALSE, FALSE, TRUE))
})
