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

test_that("`==.agg_vec` compares disaggregated values normally, with NA for missing values", {
  vd <- agg_vec(c(NA_character_, "A", "B"), aggregated = c(FALSE, FALSE, FALSE))
  ve <- agg_vec(c(NA_character_, "A", "C"), aggregated = c(FALSE, FALSE, FALSE))
  expect_equal(vd == ve, c(NA, TRUE, FALSE))
})

test_that("`==.agg_vec` gives NA for a missing value against a non-missing one", {
  vd <- agg_vec(c(NA_character_, "A"), aggregated = c(FALSE, FALSE))
  vf <- agg_vec(c("X", "A"), aggregated = c(FALSE, FALSE))
  expect_equal(vd == vf, c(NA, TRUE))
})

test_that("`==.agg_vec` never treats a genuine NA as `<aggregated>`", {
  expect_false(agg_vec(NA, FALSE) == agg_vec(NA, TRUE))
  expect_true(agg_vec(NA, FALSE) != agg_vec(NA, TRUE))
  v <- agg_vec(c(NA, NA), c(TRUE, FALSE))
  expect_equal(v == v, c(TRUE, NA))
  expect_equal(v != v, c(FALSE, NA))
  expect_equal(v == NA, c(FALSE, NA))
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

test_that("agg_vec() requires `aggregated` to match the length of `x`", {
  expect_error(agg_vec(c("A", "B"), TRUE), "same length")
  expect_error(agg_vec("A", "yes"), "same length")
})

test_that("as.character.agg_vec() returns the trimmed format", {
  v <- agg_vec(c(NA, "A", "BB"), c(TRUE, FALSE, FALSE))
  expect_equal(as.character(v), c("<aggregated>", "A", "BB"))
})

test_that("unique.agg_vec() returns an agg_vec", {
  v <- agg_vec(c(NA, "A", NA, "A", "B"), c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(duplicated(v), c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(unique(v), agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE)))
})

test_that("rep.agg_vec() keeps <aggregated> values", {
  v <- agg_vec(c(NA, "A"), c(TRUE, FALSE))
  expect_equal(format(rep(v, 2)), c("<aggregated>", "A", "<aggregated>", "A"))
})

test_that("[<- and [[<- assign values or <aggregated>", {
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  x <- v
  x[2] <- "Z"
  expect_equal(format(x), c("<aggregated>", "Z", "B"))
  x[3] <- v[1]
  expect_equal(format(x), c("<aggregated>", "Z", "<aggregated>"))
  x[1] <- "Q"
  expect_equal(format(x), c("Q", "Z", "<aggregated>"))
  x[[2]] <- v[[1]]
  expect_equal(format(x), c("Q", "<aggregated>", "<aggregated>"))
  x[5] <- "E"
  expect_equal(is.na(x), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_error(x[[1]] <- c("a", "b"), "single value")

  df <- tibble::tibble(k = v)
  df$k[1] <- "Z"
  expect_equal(format(df$k), c("Z", "A", "B"))
})

test_that("[[ and as.list() give single-element agg_vecs", {
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  expect_equal(v[[1]], v[1])
  expect_error(v[[1:2]], "one element")
  expect_error(v[[4]], "out of bounds")
  expect_equal(as.list(v), list(v[1], v[2], v[3]))
  expect_equal(vapply(v, format, character(1)), c("<aggregated>", "A", "B"))
})

test_that("agg_vec can't be plotted without format()", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(y = 1:3)
  df$k <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  p <- ggplot2::ggplot(df, ggplot2::aes(k, y)) + ggplot2::geom_col()
  expect_error(ggplot2::ggplot_build(p), "use format\\(\\)")
})

test_that("an agg_vec is a single data.frame column", {
  v <- agg_vec(c(NA, "B", "A"), c(TRUE, FALSE, FALSE))
  df <- data.frame(v = v, y = 1:3)
  expect_equal(names(df), c("v", "y"))
  expect_equal(nrow(df), 3L)
  expect_s3_class(df[2:3, ]$v, "agg_vec")
  expect_equal(format(df[2:3, ]$v), c("B", "A"))
  expect_output(print(df), "<aggregated>")
  expect_equal(names(as.data.frame(v)), "v")
})

test_that("anyDuplicated.agg_vec() agrees with duplicated()", {
  expect_equal(anyDuplicated(agg_vec(c(NA, NA), c(TRUE, TRUE))), 2L)
  expect_equal(anyDuplicated(agg_vec(c("a", NA, "a"), c(FALSE, TRUE, FALSE))), 3L)
  expect_equal(anyDuplicated(agg_vec(c("a", NA), c(FALSE, TRUE))), 0L)
  expect_equal(anyDuplicated(agg_vec(c("a", "b", "a", "b"), rep(FALSE, 4)), fromLast = TRUE), 2L)
})

test_that("match() and %in% on agg_vecs agree with vec_match()", {
  skip_if(getRversion() < "4.3.0", "match() only uses mtfrm() from R 4.3")
  x <- agg_vec(c(0.1 + 0.2, NA, 1, 2), c(FALSE, FALSE, TRUE, FALSE))
  table <- agg_vec(c(0.3, NA, 5, 2), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(match(x, table), vctrs::vec_match(x, table))
  expect_equal(match(x, table), c(NA, 2L, 3L, 4L))
  expect_equal(x %in% table, vctrs::vec_in(x, table))

  # `<aggregated>` is not the string "<aggregated>" or a missing value.
  s <- agg_vec(c("<aggregated>", "NA", NA, "a"), c(FALSE, FALSE, FALSE, TRUE))
  t <- agg_vec(c("b", NA, "<aggregated>", "NA"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(match(s, t), vctrs::vec_match(s, t))
  expect_equal(match(s, t), c(3L, 4L, 2L, 1L))
})
