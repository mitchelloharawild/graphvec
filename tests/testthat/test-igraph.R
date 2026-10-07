test_that("as.igraph() converts node_vec to an igraph object", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  ig <- igraph::as.igraph(g)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::ecount(ig), 2L)
})

test_that("as.igraph() converts agg_vec to an igraph object", {
  skip_if_not_installed("igraph")
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  ig <- igraph::as.igraph(v)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::vcount(ig), 3L)
  expect_equal(igraph::ecount(ig), 2L)
  expect_equal(igraph::as_edgelist(ig, names = FALSE), cbind(c(2L, 3L), c(1L, 1L)))
})

test_that("as.igraph() converts agg_vec as the graph nodes() gives it", {
  skip_if_not_installed("igraph")
  # Formerly a row-order "forest of stars", which disagreed with nodes().
  vs <- list(
    agg_vec(c(NA, "A", NA, "B"), aggregated = c(TRUE, FALSE, TRUE, FALSE)),
    agg_vec(c(NA, "A", "B", NA, "C"), aggregated = c(TRUE, FALSE, FALSE, TRUE, FALSE)),
    agg_vec(c("A", NA), aggregated = c(FALSE, TRUE)),
    agg_vec(c(NA, NA, "A", "B"), aggregated = c(TRUE, TRUE, FALSE, FALSE)),
    agg_vec(c("A", "B"), aggregated = c(FALSE, FALSE)),
    agg_vec()
  )
  for (v in vs) {
    expect_identical(
      igraph::as_edgelist(igraph::as.igraph(v), names = FALSE),
      igraph::as_edgelist(igraph::as.igraph(nodes(v)), names = FALSE)
    )
    expect_equal(igraph::vcount(igraph::as.igraph(v)), length(v))
  }
})

test_that("as.igraph() links every agg_vec row to the first `<aggregated>` row", {
  skip_if_not_installed("igraph")
  # Row order doesn't matter, and a later duplicate `<aggregated>` row (4) is
  # left isolated rather than starting a second star.
  v <- agg_vec(
    c(NA, "A", "B", NA, "C"),
    aggregated = c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  expect_equal(
    igraph::as_edgelist(igraph::as.igraph(v), names = FALSE),
    cbind(c(2L, 3L, 5L), c(1L, 1L, 1L))
  )

  v <- agg_vec(c("A", NA), aggregated = c(FALSE, TRUE))
  expect_equal(igraph::as_edgelist(igraph::as.igraph(v), names = FALSE), cbind(1L, 2L))

  # Consecutive `<aggregated>` rows are no longer a hyperedge error.
  v <- agg_vec(c(NA, NA, "A", "B"), aggregated = c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(
    igraph::as_edgelist(igraph::as.igraph(v), names = FALSE),
    cbind(c(3L, 4L), c(1L, 1L))
  )
})

test_that("as.igraph() errors on a hyperedge node_vec/edge_vec (igraph has no hyperedge concept)", {
  skip_if_not_installed("igraph")
  g <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_error(igraph::as.igraph(g), class = "rlang_error")

  e <- edge_vec(from = list(c(1L, 2L)), to = 3L, nodes = c("A", "B", "C"))
  expect_error(igraph::as.igraph(e), class = "rlang_error")
})

test_that("as.igraph() converts agg_df to an igraph object", {
  skip_if_not_installed("igraph")
  # A single crossed cell (row 1) with its two one-column aggregates (rows
  # 2-3) and the fully aggregated total (row 4): a diamond lattice.
  df <- agg_df(
    Purpose = agg_vec(c("Business", NA, "Business", NA), c(FALSE, TRUE, FALSE, TRUE)),
    State = agg_vec(c("NSW", "NSW", NA, NA), c(FALSE, FALSE, TRUE, TRUE))
  )
  ig <- igraph::as.igraph(df)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::vcount(ig), 4L)
  expect_equal(
    igraph::as_edgelist(ig, names = FALSE),
    cbind(c(1L, 3L, 1L, 2L), c(2L, 4L, 3L, 4L))
  )
})

test_that("as.igraph() converts edge_vec to an igraph object", {
  skip_if_not_installed("igraph")
  e <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  ig <- igraph::as.igraph(e)
  expect_s3_class(ig, "igraph")
  expect_equal(igraph::ecount(ig), 4L)
})

test_that("as.igraph() respects node_vec's `directed` attribute", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  expect_true(igraph::is_directed(igraph::as.igraph(g)))

  gu <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L),
    directed = FALSE
  )
  expect_false(igraph::is_directed(igraph::as.igraph(gu)))
})

test_that("as.igraph() respects edge_vec's `directed` attribute", {
  skip_if_not_installed("igraph")
  e <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_true(igraph::is_directed(igraph::as.igraph(e)))

  eu <- edge_vec(
    from = c(1L, 2L, 1L, 3L),
    to = c(2L, 3L, 3L, 1L),
    nodes = data.frame(label = c("A", "B", "C")),
    directed = FALSE
  )
  expect_false(igraph::is_directed(igraph::as.igraph(eu)))
})

test_that("as.igraph() counts every node of an edge_vec without node data", {
  skip_if_not_installed("igraph")
  e <- c(edge_vec(1:2, 2:3), edge_vec(1L, 2L))
  expect_equal(igraph::vcount(igraph::as.igraph(e)), 5L)
})

test_that("as.igraph() keeps a node_vec's values as the `name` vertex attribute", {
  skip_if_not_installed("igraph")
  g <- node_vec(c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L), weight = c(1.5, 2))
  ig <- igraph::as.igraph(g)
  expect_identical(igraph::vertex_attr(ig), list(name = c("A", "B", "C")))
  expect_identical(igraph::edge_attr(ig), list(weight = c(1.5, 2)))

  # Values keep their type rather than going through format().
  g <- node_vec(factor(c("a", "b")), from = 1L, to = 2L, when = as.Date("2020-01-01"))
  ig <- igraph::as.igraph(g)
  expect_identical(igraph::V(ig)$name, factor(c("a", "b")))
  expect_identical(igraph::E(ig)$when, as.Date("2020-01-01"))
  expect_identical(igraph::V(igraph::as.igraph(node_vec(1:3)))$name, 1:3)
})

test_that("as.igraph() gives each column of data-frame node values as a vertex attribute", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    data.frame(id = 1:3, label = factor(c("a", "b", "c"))),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  ig <- igraph::as.igraph(g)
  expect_identical(
    igraph::vertex_attr(ig),
    list(id = 1:3, label = factor(c("a", "b", "c")))
  )
  expect_length(igraph::edge_attr(ig), 0L)
})

test_that("as.igraph() aligns attributes with a node_vec slice's induced subgraph", {
  skip_if_not_installed("igraph")
  g <- node_vec(
    c("A", "B", "C"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 1L),
    weight = c(1, 2, 3)
  )
  # B is repeated, so its edge to C is cloned; the edges touching A are gone.
  ig <- igraph::as.igraph(g[c(3L, 2L, 2L)])
  expect_identical(igraph::V(ig)$name, c("C", "B", "B"))
  expect_equal(igraph::as_edgelist(ig, names = FALSE), cbind(c(2L, 3L), c(1L, 1L)))
  expect_identical(igraph::E(ig)$weight, c(2, 2))

  ig <- igraph::as.igraph(g[c(1L, 3L)])
  expect_identical(igraph::V(ig)$name, c("A", "C"))
  expect_identical(igraph::E(ig)$weight, 3)
})

test_that("as.igraph() keeps an edge_vec's node data and edge attributes", {
  skip_if_not_installed("igraph")
  e <- edge_vec(
    from = c(1L, 2L, 1L),
    to = c(2L, 3L, 3L),
    weight = c(1, 2, 5),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  ig <- igraph::as.igraph(e[c(3L, 1L)])
  # Every node of the graph, but only the slice's edges, in its order.
  expect_identical(igraph::vertex_attr(ig), list(label = c("A", "B", "C")))
  expect_equal(igraph::as_edgelist(ig, names = FALSE), cbind(c(1L, 1L), c(3L, 2L)))
  expect_identical(igraph::edge_attr(ig), list(weight = c(5, 1)))

  ig <- igraph::as.igraph(edge_vec(1L, 2L, nodes = c("x", "y")))
  expect_identical(igraph::vertex_attr(ig), list(name = c("x", "y")))

  # No node data gives no vertex attributes.
  ig <- igraph::as.igraph(edge_vec(1:2, 2:3))
  expect_length(igraph::vertex_attr(ig), 0L)
  expect_length(igraph::edge_attr(ig), 0L)
})

test_that("as.igraph() gives an agg_vec/agg_df's columns as vertex attributes", {
  skip_if_not_installed("igraph")
  v <- agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE))
  ig <- igraph::as.igraph(v)
  expect_identical(igraph::vertex_attr(ig), list(value = v))
  expect_identical(
    igraph::vertex_attr(ig),
    igraph::vertex_attr(igraph::as.igraph(nodes(v)))
  )

  purpose <- agg_vec(c("Business", NA, "Business", NA), c(FALSE, TRUE, FALSE, TRUE))
  state <- agg_vec(c("NSW", "NSW", NA, NA), c(FALSE, FALSE, TRUE, TRUE))
  ig <- igraph::as.igraph(agg_df(Purpose = purpose, State = state))
  expect_identical(igraph::vertex_attr(ig), list(Purpose = purpose, State = state))
})
