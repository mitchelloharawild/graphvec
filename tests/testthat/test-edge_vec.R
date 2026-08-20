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
    "only supports `from`, `to`, or an edge attribute, not `foo`"
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

test_that("type_sum.edge_vec() abbreviates the node data type", {
  skip_if_not_installed("pillar")
  e <- edge_vec(from = 1L, to = 2L, nodes = c("A", "B"))
  expect_equal(pillar::type_sum(e), "E[chr]")

  e <- edge_vec(from = 1L, to = 2L, nodes = factor(c("A", "B")))
  expect_equal(pillar::type_sum(e), "E[fct]")

  # A data-frame-backed `nodes` shouldn't double up pillar::type_sum()'s own
  # "[,ncol]" shape suffix inside ours, e.g. "E[df[,1]]".
  e <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("A", "B")))
  expect_equal(pillar::type_sum(e), "E[df]")
})

test_that("c.edge_vec() is a disjoint union: nodes concatenate, second graph's from/to are offset", {
  e1 <- edge_vec(from = 1L, to = 2L, weight = 1, nodes = data.frame(label = c("A", "B")))
  e2 <- edge_vec(from = 1L, to = 2L, weight = 2, nodes = data.frame(label = c("X", "Y")))
  u <- c(e1, e2)

  expect_s3_class(u, "edge_vec")
  expect_equal(format(u), c("[A]->[B]", "[X]->[Y]"))
  expect_equal(attr(u, "nodes"), data.frame(label = c("A", "B", "X", "Y")))
  expect_equal(u$weight, c(1, 2))
})

test_that("c.edge_vec() combines more than two edge_vec objects in order", {
  e1 <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("A", "B")))
  e2 <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("X", "Y")))
  e3 <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("P", "Q")))
  u <- c(e1, e2, e3)

  expect_equal(format(u), c("[A]->[B]", "[X]->[Y]", "[P]->[Q]"))
  expect_equal(attr(u, "nodes"), data.frame(label = c("A", "B", "X", "Y", "P", "Q")))
})

test_that("c.edge_vec() keeps a zero-edge source's isolated nodes in the union", {
  # c.edge_vec() combines the `nodes` tables directly rather than through a
  # one-row-per-edge proxy, so a source with no edges still contributes its
  # nodes.
  e1 <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("A", "B")))
  e2 <- edge_vec(from = integer(), to = integer(), nodes = data.frame(label = "X"))
  u <- c(e1, e2)

  expect_equal(format(u), "[A]->[B]")
  expect_equal(attr(u, "nodes"), data.frame(label = c("A", "B", "X")))
})

test_that("c.edge_vec() pads a missing edge attribute with NA when combining edge_vec objects", {
  e1 <- edge_vec(from = 1L, to = 2L, weight = 5, nodes = data.frame(label = c("A", "B")))
  e2 <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("X", "Y")))
  u <- c(e1, e2)

  expect_equal(u$weight, c(5, NA))
})

test_that("c.edge_vec() rejects combining edge_vec objects with different `directed`", {
  e <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("A", "B")))
  expect_error(c(e, edge_vec(directed = FALSE)), "directed")
})

test_that("edge_vec() defaults to directed = TRUE and stores it as an attribute", {
  e <- edge_vec(from = 1L, to = 2L)
  expect_true(attr(e, "directed"))

  eu <- edge_vec(from = 1L, to = 2L, directed = FALSE)
  expect_false(attr(eu, "directed"))
})

test_that("edge_vec() validates directed as a single non-NA logical", {
  expect_error(edge_vec(directed = NA))
  expect_error(edge_vec(directed = c(TRUE, FALSE)))
  expect_error(edge_vec(directed = "TRUE"))
})

test_that("directed survives slicing an edge_vec", {
  eu <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), directed = FALSE)
  expect_false(attr(eu[1], "directed"))
})

test_that("edge_vec() accepts any base vector as nodes, not just a data frame", {
  e <- edge_vec(from = 1L, to = 2L, nodes = c("A", "B"))
  expect_s3_class(e, "edge_vec")
  expect_equal(attr(e, "nodes"), c("A", "B"))
  expect_equal(format(e), "[A]->[B]")

  ef <- edge_vec(from = 1L, to = 2L, nodes = factor(c("A", "B")))
  expect_equal(format(ef), "[A]->[B]")
})

test_that("edge_vec() still rejects non-vector nodes", {
  expect_error(edge_vec(from = 1L, to = 2L, nodes = function() NULL))
})

test_that("edge_vec() accepts named edge attributes via ..., recycled to the edge count", {
  e <- edge_vec(
    from = c(1L, 2L),
    to = c(2L, 3L),
    weight = c(10, 20),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_equal(e$weight, c(10, 20))

  e1 <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), weight = 5)
  expect_equal(e1$weight, c(5, 5))
})

test_that("edge_vec() rejects unnamed edge attributes", {
  expect_error(edge_vec(from = 1L, to = 2L, 5), "must be named")
})

test_that("edge attributes are kept in sync under `[` slicing", {
  e <- edge_vec(from = c(1L, 2L, 3L), to = c(2L, 3L, 4L), weight = c(1, 2, 3))
  expect_equal(e[2:3]$weight, c(2, 3))
})

test_that("as_tibble()/as.data.frame() on an edge_vec show from/to and attribute columns", {
  e <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), weight = c(10, 20))

  tbl <- tibble::as_tibble(e)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl, tibble::tibble(from = c(1L, 2L), to = c(2L, 3L), weight = c(10, 20)))

  df <- as.data.frame(e)
  expect_equal(df, data.frame(from = c(1L, 2L), to = c(2L, 3L), weight = c(10, 20)))
})
