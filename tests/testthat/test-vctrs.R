skip_if_not_installed("vctrs")

edge_pairs <- function(e) {
  paste0(e[["from"]], "->", e[["to"]])
}

# -- agg_vec --------------------------------------------------------------

test_that("agg_vec is a vctrs vector", {
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  expect_true(vctrs::obj_is_vector(v))
  expect_equal(vctrs::vec_size(v), 3L)
  expect_equal(vctrs::vec_ptype_abbr(v), "chr*")
  expect_s3_class(tibble::tibble(k = v)$k, "agg_vec")
})

test_that("vec_slice() and vec_c() keep <aggregated> values", {
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  s <- vctrs::vec_slice(v, c(3, 1))
  expect_equal(format(s), c("B", "<aggregated>"))

  out <- vctrs::vec_c(v, v)
  expect_equal(is_aggregated(out), rep(c(TRUE, FALSE, FALSE), 2))
  expect_equal(vctrs::vec_unique(out), v)
  expect_equal(vctrs::vec_group_id(out), c(1, 2, 3, 1, 2, 3), ignore_attr = TRUE)
})

test_that("agg_vec combines with character in either order", {
  v <- agg_vec(c(NA, "A"), c(TRUE, FALSE))
  expect_equal(format(vctrs::vec_c(v, "Z")), c("<aggregated>", "A", "Z"))
  expect_equal(format(vctrs::vec_c("Z", v)), c("Z", "<aggregated>", "A"))
  expect_error(vctrs::vec_c(v, 1), class = "vctrs_error_incompatible_type")
})

test_that("an all-<aggregated> agg_vec takes on the other value type", {
  v <- agg_vec(c(NA, "A"), c(TRUE, FALSE))
  all_agg <- agg_vec(NA, TRUE)
  out <- vctrs::vec_c(all_agg, v)
  expect_equal(format(out), c("<aggregated>", "<aggregated>", "A"))
  expect_type(agg_vec_values(out), "character")
  expect_equal(format(vctrs::vec_c(v, all_agg)), c("<aggregated>", "A", "<aggregated>"))
})

test_that("casting agg_vec to character is lossy only for <aggregated>", {
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  expect_equal(vctrs::vec_cast(v[2:3], character()), c("A", "B"))
  expect_error(vctrs::vec_cast(v, character()), class = "vctrs_error_cast_lossy")
})

test_that("vctrs sees missing disaggregated values, not <aggregated>, as missing", {
  v <- agg_vec(c("B", NA, NA), c(FALSE, TRUE, FALSE))
  expect_equal(vctrs::vec_detect_missing(v), c(FALSE, FALSE, TRUE))
  expect_equal(format(vctrs::vec_init(v, 1)), "NA")
})

test_that("<aggregated> sorts last", {
  v <- agg_vec(c(NA, "B", "A"), c(TRUE, FALSE, FALSE))
  expect_equal(vctrs::vec_order(v), c(3L, 2L, 1L))
  expect_equal(format(sort(v)), c("A", "B", "<aggregated>"))
  expect_equal(order(v), c(3L, 2L, 1L))
})

test_that("agg_vec works with dplyr verbs", {
  skip_if_not_installed("dplyr")
  df <- tibble::tibble(k = agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE)), y = 1:3)

  bound <- dplyr::bind_rows(df, df)
  expect_equal(is_aggregated(bound$k), rep(c(TRUE, FALSE, FALSE), 2))
  expect_equal(format(dplyr::filter(df, y > 1)$k), c("A", "B"))
  expect_equal(format(dplyr::arrange(df, k)$k), c("A", "B", "<aggregated>"))

  counted <- dplyr::count(bound, k)
  expect_equal(format(counted$k), c("A", "B", "<aggregated>"))
  expect_equal(counted$n, c(2L, 2L, 2L))
})

test_that("agg_vec works as a tsibble key", {
  skip_if_not_installed("tsibble")
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  ts <- tsibble::tsibble(k = rep(v, each = 2), t = rep(1:2, 3), key = k, index = t)
  expect_equal(nrow(tsibble::key_data(ts)), 3L)
})

# -- node_vec -------------------------------------------------------------

test_that("node_vec keeps its edges through vctrs", {
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  expect_equal(vctrs::vec_ptype_abbr(n), "N[chr]")
  expect_equal(edge_pairs(attr(vctrs::vec_slice(n, c(3, 1, 2)), "edges")), c("2->3", "3->1"))
  expect_equal(
    edge_pairs(attr(vctrs::vec_c(n, n), "edges")),
    c("1->2", "2->3", "4->5", "5->6")
  )
})

test_that("vec_c() of node_vecs keeps row order across different graphs", {
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  m <- new_node_vec(c("X", "Y"), edges = data.frame(from = 2L, to = 1L))
  out <- vctrs::vec_c(n, m, n)
  expect_equal(format(out), c("A", "B", "C", "X", "Y", "A", "B", "C"))
  expect_equal(attr(out, "edges"), attr(c(n, m, n), "edges"))
})

test_that("bind_rows() keeps node_vec edges", {
  skip_if_not_installed("dplyr")
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  df <- tibble::tibble(i = n, y = 1:3)

  expect_equal(
    edge_pairs(attr(dplyr::bind_rows(df, df)$i, "edges")),
    c("1->2", "2->3", "4->5", "5->6")
  )
  expect_equal(edge_pairs(attr(dplyr::arrange(df, dplyr::desc(y))$i, "edges")), c("3->2", "2->1"))
  expect_equal(nrow(attr(dplyr::filter(df, y != 2)$i, "edges")), 0L)
})

test_that("node_vec equality is value-based", {
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  expect_equal(format(vctrs::vec_unique(vctrs::vec_c(n, n))), c("A", "B", "C"))
  expect_equal(vctrs::vec_order(n[c(3, 1, 2)]), c(2L, 3L, 1L))
})

test_that("node_vecs with different `directed` can't be combined", {
  n <- node_vec("A")
  expect_error(
    vctrs::vec_c(n, node_vec("B", directed = FALSE)),
    class = "vctrs_error_incompatible_type"
  )
})

# -- edge_vec -------------------------------------------------------------

test_that("edge_vec is a vctrs vector", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  expect_true(vctrs::obj_is_vector(e))
  expect_equal(vctrs::vec_size(e), 2L)
  expect_equal(vctrs::vec_ptype_abbr(e), "E[chr]")
  expect_equal(format(tibble::tibble(e = e)$e), c("[A]->[B]", "[B]->[C]"))
  expect_equal(format(vctrs::vec_slice(e, 2)), "[B]->[C]")
})

test_that("vec_c() of edge_vecs matches c()", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  f <- edge_vec(1L, 2L, w = 9, nodes = c("X", "Y"))
  expect_equal(vctrs::vec_c(e, f, e), c(e, f, e))
  expect_equal(vctrs::vec_c(e, e), c(e, e))
})

test_that("vec_c() combines ordinary and hyperedge edge_vecs", {
  e <- edge_vec(1:2, 2:3, nodes = c("A", "B", "C"))
  h <- edge_vec(list(1:2), 3L, nodes = c("A", "B", "C"))
  expect_equal(vctrs::vec_c(e, h), c(e, h))
  expect_equal(format(tibble::tibble(h = h)$h), "[{A,B}]->[C]")
})

test_that("edge_vec works with dplyr verbs", {
  skip_if_not_installed("dplyr")
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  df <- tibble::tibble(e = e, y = 1:2)
  expect_equal(dplyr::bind_rows(df, df)$e, c(e, e))
  expect_equal(dplyr::filter(df, y == 2)$e, e[2])
})

test_that("edge_vecs with different `directed` can't be combined", {
  e <- edge_vec(1L, 2L, nodes = c("A", "B"))
  expect_error(
    vctrs::vec_c(e, edge_vec(1L, 2L, nodes = c("A", "B"), directed = FALSE)),
    class = "vctrs_error_incompatible_type"
  )
})
