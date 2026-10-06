skip_if_not_installed("vctrs")

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

test_that("<aggregated> is complete and unequal to any value", {
  v <- agg_vec(c(NA, "A", NA), c(TRUE, FALSE, FALSE))
  expect_equal(vctrs::vec_detect_complete(v), c(TRUE, TRUE, FALSE))
  expect_equal(vctrs::vec_equal(v, "A"), c(FALSE, TRUE, NA))
  # `<aggregated>` and a missing value are distinct groups.
  expect_equal(vctrs::vec_unique_count(vctrs::vec_c(v, v)), 3L)
  expect_true(vctrs::vec_equal(v[1], agg_vec(NA, TRUE)))

  for (vals in list(c(NA, 1L), c(NA, 1.5), c(NA, TRUE), as.Date(c(NA, "2020-01-01")),
                    factor(c(NA, "x")), as.POSIXlt(c(NA, "2020-01-01")), list(NULL, 1))) {
    x <- agg_vec(vals, c(TRUE, FALSE))
    expect_equal(vctrs::vec_detect_complete(x), c(TRUE, TRUE))
    expect_equal(vctrs::vec_unique_count(vctrs::vec_c(x, x)), 2L)
  }
  expect_equal(vctrs::vec_detect_complete(agg_vec(c(NA, NA), c(TRUE, TRUE))), c(TRUE, TRUE))
})

test_that("drop_na() keeps <aggregated> rows", {
  skip_if_not_installed("tidyr")
  df <- tibble::tibble(k = agg_vec(c(NA, "A", NA), c(TRUE, FALSE, FALSE)))
  expect_equal(format(tidyr::drop_na(df)$k), c("<aggregated>", "A"))
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
  expect_equal(edge_pairs(vctrs::vec_slice(n, c(3, 1, 2))), c("2->3", "3->1"))
  expect_equal(
    edge_pairs(vctrs::vec_c(n, n)),
    c("1->2", "2->3", "4->5", "5->6")
  )
})

test_that("vec_c() of node_vecs keeps row order across different graphs", {
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  m <- new_node_vec(c("X", "Y"), edges = data.frame(from = 2L, to = 1L))
  out <- vctrs::vec_c(n, m, n)
  expect_equal(format(out), c("A", "B", "C", "X", "Y", "A", "B", "C"))
  expect_equal(out, c(n, m, n))
})

test_that("bind_rows() keeps node_vec edges", {
  skip_if_not_installed("dplyr")
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  df <- tibble::tibble(i = n, y = 1:3)

  expect_equal(
    edge_pairs(dplyr::bind_rows(df, df)$i),
    c("1->2", "2->3", "4->5", "5->6")
  )
  expect_equal(edge_pairs(dplyr::arrange(df, dplyr::desc(y))$i), c("3->2", "2->1"))
  expect_length(edges(dplyr::filter(df, y != 2)$i), 0L)
})

test_that("node_vec equality is value-based", {
  n <- new_node_vec(c("A", "B", "C"), edges = data.frame(from = 1:2, to = 2:3))
  expect_equal(format(vctrs::vec_unique(vctrs::vec_c(n, n))), c("A", "B", "C"))
  expect_equal(vctrs::vec_order(n[c(3, 1, 2)]), c(2L, 3L, 1L))
})

test_that("a data-frame-backed node_vec combines through vctrs", {
  skip_if_not_installed("dplyr")
  nd <- node_vec(data.frame(id = 1:3, lab = c("A", "B", "C")), from = 1:2, to = 2:3)
  expect_null(names(nd))
  expect_equal(vctrs::vec_c(nd, nd), c(nd, nd))
  expect_equal(vctrs::list_unchop(list(nd, nd)), c(nd, nd))

  df <- tibble::tibble(x = nd, y = 1:3)
  expect_equal(dplyr::bind_rows(df, df)$x, c(nd, nd))
  expect_equal(nrow(dplyr::left_join(df, df, by = "x")), 3L)
  expect_equal(format(dplyr::summarise(df, z = dplyr::first(x))$z), "1:A")
  expect_equal(format(dplyr::coalesce(nd, nd)), format(nd))

  # One column, and as many columns as rows.
  n1 <- node_vec(data.frame(id = 1:3), from = 1L, to = 2L)
  expect_equal(vctrs::vec_c(n1, n1), c(n1, n1))
  n2 <- node_vec(data.frame(id = 1:2, lab = c("A", "B")), from = 1L, to = 2L)
  expect_null(names(vctrs::vec_c(n2, n2)))
})

test_that("names(x) <- NULL keeps a data-frame-backed node_vec's columns", {
  nd <- node_vec(data.frame(id = 1:2, lab = c("A", "B")))
  names(nd) <- NULL
  expect_equal(format(nd), c("1:A", "2:B"))
  expect_error(names(nd) <- c("a", "b"), "can't have names")
})

test_that("atomic node_vecs keep their element names", {
  n <- node_vec(c(a = "A", b = "B"), 1L, 2L)
  expect_equal(names(n), c("a", "b"))
  expect_equal(names(n[2:1]), c("b", "a"))
  names(n) <- c("p", "q")
  expect_equal(names(n), c("p", "q"))
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

test_that("edge_vec equality is by node values, not positions", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  g <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("X", "Y", "Z"))
  expect_equal(vctrs::vec_equal(e, g), c(FALSE, FALSE))
  expect_equal(vctrs::vec_unique_count(vctrs::vec_c(e, e)), 2L)
  expect_equal(vctrs::vec_unique_count(vctrs::vec_c(e, g)), 4L)
  expect_equal(vctrs::vec_unique_count(vctrs::vec_c(e, edge_vec(1:2, 2:3, w = 0, nodes = c("A", "B", "C")))), 4L)

  ed <- edge_vec(1:2, 2:3, nodes = data.frame(id = 1:3, lab = c("A", "B", "C")))
  expect_equal(vctrs::vec_unique_count(vctrs::vec_c(ed, ed)), 2L)
  h <- edge_vec(list(1:2, 3L), list(3L, 1L), nodes = c("A", "B", "C"))
  expect_equal(vctrs::vec_unique_count(vctrs::vec_c(h, h)), 2L)
  # With no node data, positions are all there is to compare.
  e0 <- edge_vec(1:2, 2:3)
  expect_equal(vctrs::vec_equal(e0, edge_vec(c(1L, 3L), 2:3)), c(TRUE, FALSE))
})

test_that("edge_vec columns can be join keys", {
  skip_if_not_installed("dplyr")
  e <- edge_vec(1:2, 2:3, nodes = c("A", "B", "C"))
  g <- edge_vec(1L, 2L, nodes = c("X", "Y"))
  joined <- dplyr::inner_join(tibble::tibble(e = e, y = 1:2), tibble::tibble(e = e, z = 2:1), by = "e")
  expect_equal(joined$z, 2:1)
  expect_equal(nrow(dplyr::semi_join(tibble::tibble(e = c(e, g)), tibble::tibble(e = e[2]), by = "e")), 1L)
  expect_equal(dplyr::count(tibble::tibble(e = c(e, e)), e)$n, c(2L, 2L))

  h <- edge_vec(list(1:2, 3L), list(3L, 1L), nodes = c("A", "B", "C"))
  expect_equal(nrow(dplyr::inner_join(tibble::tibble(e = h), tibble::tibble(e = h), by = "e")), 2L)
})

test_that("edge_vecs sort by node values, including hyperedges", {
  skip_if_not_installed("dplyr")
  e <- edge_vec(c(2L, 1L, 1L), c(3L, 3L, 2L), nodes = c("A", "B", "C"))
  expect_equal(format(vctrs::vec_sort(e)), c("[A]->[B]", "[A]->[C]", "[B]->[C]"))

  h <- edge_vec(list(1:2, 3L, 1L), list(3L, 1L, 2L), nodes = c("A", "B", "C"))
  expect_equal(
    format(dplyr::arrange(tibble::tibble(h = h), h)$h),
    c("[A]->[B]", "[{A,B}]->[C]", "[C]->[A]")
  )
  expect_error(vctrs::vec_compare(h, h), "Can't compare hyperedges")
})

test_that("[<- on an edge_vec matches vec_assign()", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  g <- edge_vec(1L, 2L, w = 9, nodes = c("X", "Y"))
  x <- e
  x[1] <- g
  expect_equal(vctrs::vec_equal(x, vctrs::vec_assign(e, 1L, g)), c(TRUE, TRUE))
})

test_that("purrr::map() iterates over agg_vec and edge_vec elements", {
  skip_if_not_installed("purrr")
  v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
  expect_equal(purrr::map_chr(v, format), c("<aggregated>", "A", "B"))
  e <- edge_vec(1:2, 2:3, nodes = c("A", "B", "C"))
  expect_equal(purrr::map_chr(e, format), c("[A]->[B]", "[B]->[C]"))
})

test_that("edge_vecs with different `directed` can't be combined", {
  e <- edge_vec(1L, 2L, nodes = c("A", "B"))
  expect_error(
    vctrs::vec_c(e, edge_vec(1L, 2L, nodes = c("A", "B"), directed = FALSE)),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("vec_c() of edge_vecs without node data matches c()", {
  e <- edge_vec(1:2, 2:3)
  f <- edge_vec(1L, 2L)
  expect_equal(vctrs::vec_c(e, f), c(e, f))
  expect_equal(format(vctrs::vec_init(e, 1)), "[NA]->[NA]")
})

test_that("vec_init() of a hyperedge edge_vec is missing", {
  h <- edge_vec(list(1:2, 3L, 1L), list(3L, 1L, 2:3), w = 1:3, nodes = c("A", "B", "C"))
  init <- vctrs::vec_init(h, 1)
  expect_equal(format(init), "[NA]->[NA]")
  expect_true(vctrs::vec_detect_missing(init))
  expect_true(is.na(init))
  x <- vctrs::vec_c(h, init)
  expect_equal(vctrs::vec_detect_missing(x), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(is.na(x), vctrs::vec_detect_missing(x))

  # Likewise without node data.
  init <- vctrs::vec_init(edge_vec(list(1:2), list(3L)), 1)
  expect_equal(format(init), "[NA]->[NA]")
  expect_true(vctrs::vec_detect_missing(init))
})

test_that("is.na() of an edge_vec agrees with vec_detect_missing()", {
  e <- edge_vec(1:2, 2:3, w = 1:2, nodes = c("A", "B", "C"))
  x <- vctrs::vec_c(e, vctrs::vec_init(e, 1))
  expect_equal(is.na(x), c(FALSE, FALSE, TRUE))
  expect_equal(is.na(x), vctrs::vec_detect_missing(x))
})

test_that("lag(), drop_na() and fill() treat missing hyperedges as missing", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  h <- edge_vec(list(1:2, 3L, 1L), list(3L, 1L, 2:3), nodes = c("A", "B", "C"))
  df <- dplyr::mutate(tibble::tibble(e = h, x = 1:3), l = dplyr::lag(e))
  expect_equal(format(df$l), c("[NA]->[NA]", "[{A,B}]->[C]", "[C]->[A]"))
  expect_equal(tidyr::drop_na(df)$x, 2:3)
  filled <- tidyr::fill(df, l, .direction = "up")
  expect_equal(format(filled$l), c("[{A,B}]->[C]", "[{A,B}]->[C]", "[C]->[A]"))
})
