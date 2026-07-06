test_that("edge_vec() returns an edge_vec object", {
  e <- edge_vec(
    from = c(1L, 2L, 1L),
    to = c(2L, 3L, 3L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_s3_class(e, "edge_vec")
})

test_that("edge_vec() preserves the number of edges", {
  e <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_length(e, 4L)
})

test_that("edge_vec() stores node data as an attribute", {
  nodes <- data.frame(label = c("A", "B", "C"))
  e <- edge_vec(from = 1L, to = 2L, nodes = nodes)
  expect_equal(attr(e, "nodes"), nodes)
})

test_that("edge_vec() accepts an empty vector", {
  e <- edge_vec()
  expect_s3_class(e, "edge_vec")
  expect_length(e, 0L)
})

test_that("new_edge_vec() is a low-level constructor for edge_vec", {
  e <- new_edge_vec(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_s3_class(e, "edge_vec")
})

test_that("$.edge_vec retrieves node data for from and to", {
  e <- edge_vec(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_equal(e$from$label, c("A", "B"))
  expect_equal(e$to$label, c("B", "C"))
})

test_that("$.edge_vec rejects invalid field names", {
  e <- edge_vec(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_error(
    e$foo,
    "only supports `from` and `to`, not `foo`"
  )
})

test_that("format.edge_vec() produces [from]->[to] strings", {
  e <- edge_vec(
    from = 1L,
    to = 2L,
    nodes = data.frame(label = c("A", "B"))
  )
  expect_equal(format(e), "[A]->[B]")
})

test_that("vec_cast() to edge_vec rejects incompatible inputs", {
  expect_error(vec_cast("A", edge_vec()), class = "vctrs_error_cast")
})
