skip_if_not_installed("waldo")

test_that("node_vecs compare by contents, not GraphBackend pointer", {
  n <- node_vec(c("a", "b", "c"), from = 1:2, to = 2:3, w = c(1, 2))
  expect_equal(c(n, n), vctrs::vec_c(n, n))
  expect_equal(n, node_vec(c("a", "b", "c"), from = 1:2, to = 2:3, w = c(1, 2)))
})

test_that("edge_vecs compare by contents, not GraphBackend pointer or edge_id", {
  nodes <- c("a", "b", "c")
  e <- edge_vec(from = 1:2, to = 2:3, w = c(5, 6), nodes = nodes)
  expect_equal(c(e, e), vctrs::vec_c(e, e))
  expect_equal(e[2], edge_vec(from = 2L, to = 3L, w = 6, nodes = nodes))
})

test_that("different node_vecs are still reported as different", {
  n <- node_vec(c("a", "b", "c"), from = 1:2, to = 2:3, w = c(1, 2))
  expect_failure(expect_equal(n, node_vec(c("a", "b", "c"), from = 1:2, to = c(3L, 3L), w = c(1, 2))))
  expect_failure(expect_equal(n, node_vec(c("a", "b", "c"), from = 1:2, to = 2:3, w = c(1, 3))))
  expect_failure(expect_equal(n, node_vec(c("a", "b", "d"), from = 1:2, to = 2:3, w = c(1, 2))))
  expect_failure(expect_equal(n, node_vec(c("a", "b", "c"), from = 1:2, to = 2:3, w = c(1, 2), directed = FALSE)))
  expect_true(length(waldo::compare(n, n[1:2])) > 0)
})

test_that("different edge_vecs are still reported as different", {
  nodes <- c("a", "b", "c")
  e <- edge_vec(from = 1:2, to = 2:3, w = c(5, 6), nodes = nodes)
  expect_failure(expect_equal(e, edge_vec(from = 1:2, to = c(3L, 3L), w = c(5, 6), nodes = nodes)))
  expect_failure(expect_equal(e, edge_vec(from = 1:2, to = 2:3, w = c(5, 7), nodes = nodes)))
  expect_failure(expect_equal(e, edge_vec(from = 1:2, to = 2:3, w = c(5, 6), nodes = c("a", "b", "d"))))
  expect_failure(expect_equal(e, edge_vec(from = 1:2, to = 2:3, w = c(5, 6), nodes = nodes, directed = FALSE)))
  expect_true(length(waldo::compare(e[1], e[2])) > 0)
})

test_that("hyperedge edge_vecs compare by contents", {
  nodes <- c("a", "b", "c")
  h <- edge_vec(from = list(1:2, 3L), to = list(3L, 1:2), nodes = nodes)
  expect_equal(c(h, h), vctrs::vec_c(h, h))
  expect_equal(h[2], edge_vec(from = list(3L), to = list(1:2), nodes = nodes))
  expect_failure(expect_equal(h[2], edge_vec(from = list(3L), to = list(1L), nodes = nodes)))
})
