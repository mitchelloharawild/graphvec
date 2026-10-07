# A GraphBackend's external pointer comes back null from serialize()/
# saveRDS() and in callr/future workers; graph_of() rebuilds the graph from
# the source kept in the pointer's protected slot, with the same uid, so a
# reloaded vector keeps working and keeps its graph identity.

round_trip <- function(x) unserialize(serialize(x, NULL))

rds_trip <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(x, path)
  readRDS(path)
}

test_that("a node_vec works after serialize() and saveRDS()", {
  n <- node_vec(c("A", "B", "C", "D"), from = c(1L, 2L, 3L), to = c(2L, 3L, 1L))
  for (m in list(round_trip(n), rds_trip(n))) {
    expect_equal(node_degree(m), node_degree(n))
    expect_equal(format(edges(m)), format(edges(n)))
    expect_equal(format(m), format(n))
    expect_output(print(m), "node_vec")
    expect_true(all(vctrs::vec_equal(m, n)))
  }
})

test_that("a node_vec slice works after serialize() and saveRDS()", {
  n <- node_vec(c("A", "B", "C", "D"), from = c(1L, 2L, 3L), to = c(2L, 3L, 1L))
  s <- n[2:3]
  for (m in list(round_trip(s), rds_trip(s))) {
    expect_equal(node_degree(m), node_degree(s))
    expect_equal(format(edges(m)), format(edges(s)))
    expect_true(all(vctrs::vec_equal(m, n[2:3])))
    # Still a slice of `n`: combining puts it back with `n`'s other nodes.
    expect_equal(format(edges(c(n[1], m))), format(edges(n[1:3])))
  }
})

test_that("an edge_vec and its slices work after serialize() and saveRDS()", {
  e <- edge_vec(c(1L, 2L, 3L), c(2L, 3L, 1L), nodes = c("A", "B", "C"))
  for (x in list(e, e[2:3], e[0])) {
    for (y in list(round_trip(x), rds_trip(x))) {
      expect_equal(format(y), format(x))
      expect_output(print(y), "edge_vec")
      expect_equal(format(nodes(y)), format(nodes(x)))
      expect_equal(node_degree(nodes(y)), node_degree(nodes(x)))
      expect_equal(vctrs::vec_equal(y, x), rep(TRUE, length(x)))
    }
  }
  # Same graph as `e`, so c() shares it rather than copying the nodes.
  y <- round_trip(e[2])
  expect_equal(NROW(attr(c(e, y), "nodes")), 3L)
  expect_true(vctrs::vec_in(y, e))
})

test_that("objects saved together stay the same graph", {
  n <- node_vec(c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  l <- round_trip(list(n, n[3], edges(n)))
  expect_true(all(vctrs::vec_equal(l[[1]][3], l[[2]])))
  expect_equal(format(edges(c(l[[1]][1:2], l[[2]]))), format(edges(n)))
  expect_equal(format(nodes(l[[3]])), format(n))

  # And so do two separate reloads of the same save.
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(n, path)
  a <- readRDS(path)
  b <- readRDS(path)
  expect_true(all(vctrs::vec_equal(a, b)))
  expect_equal(format(edges(c(a[1:2], b[3]))), format(edges(n)))
})

test_that("distinct graphs stay distinct after a reload", {
  n <- node_vec(c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  k <- node_vec(c("A", "B", "C"), from = c(2L, 1L), to = c(3L, 2L))
  l <- round_trip(list(n, k))
  # identical() before anything is rebuilt, when both pointers are null.
  expect_false(identical(l[[1]], l[[2]]))
  expect_false(any(vctrs::vec_equal(l[[1]], l[[2]])))
  expect_false(any(vctrs::vec_equal(l[[1]], k)))
  expect_equal(length(edges(c(l[[1]], l[[2]]))), 4L)

  e <- edge_vec(1L, 2L, nodes = c("A", "B"))
  f <- edge_vec(1L, 2L, nodes = c("A", "B"))
  l <- round_trip(list(e, f))
  expect_false(identical(l[[1]], l[[2]]))
  expect_false(vctrs::vec_equal(l[[1]], l[[2]]))
  expect_equal(NROW(attr(c(l[[1]], l[[2]]), "nodes")), 4L)
})

test_that("an agg_vec and agg_df work after serialize()", {
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  w <- round_trip(v)
  expect_equal(format(w), format(v))
  expect_equal(format(edges(nodes(w))), format(edges(nodes(v))))
  df <- agg_df(x = v)
  expect_equal(format(edges(nodes(round_trip(df)))), format(edges(nodes(df))))
})

test_that("building a graph leaves the RNG state alone", {
  set.seed(1)
  seed <- .Random.seed
  node_vec(1:3, from = 1L, to = 2L)
  edge_vec(1L, 2L)
  expect_identical(.Random.seed, seed)
})

test_that("a node_vec and edge_vec work in a callr worker", {
  skip_on_cran()
  skip_if_not_installed("callr")
  n <- node_vec(c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  out <- callr::r(
    function(x) list(degree = graphvec::node_degree(x), x = x, e = graphvec::edges(x)),
    list(x = n)
  )
  expect_equal(out$degree, node_degree(n))
  expect_equal(format(out$e), format(edges(n)))
  expect_true(all(vctrs::vec_equal(out$x, n)))
  expect_true(all(vctrs::vec_equal(out$e, edges(n))))
})
