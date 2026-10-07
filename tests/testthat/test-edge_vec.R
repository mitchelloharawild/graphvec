test_that("edge_vec() returns an edge_vec object", {
  e <- edge_vec(
    from = c(1L, 2L, 1L),
    to = c(2L, 3L, 3L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_s3_class(e, "edge_vec")
})

test_that("format() labels endpoints by position when there's no node data", {
  expect_equal(format(edge_vec(1:2, 2:3)), c("[1]->[2]", "[2]->[3]"))
  expect_equal(format(edge_vec(1:2, 2:3, directed = FALSE)), c("[1]--[2]", "[2]--[3]"))
  expect_equal(
    format(edge_vec(list(1:2, integer()), list(3L, 1L))),
    c("[{1,2}]->[3]", "[{}]->[1]")
  )
  expect_equal(format(edge_vec(1:2, 2:3)[2]), "[2]->[3]")
  expect_equal(format(edge_vec()), character())
  expect_output(print(edge_vec(1L, 2L)), "[1]->[2]", fixed = TRUE)
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

test_that("$.edge_vec retrieves node data for from and to when nodes is a plain vector", {
  e <- edge_vec(from = c(1L, 2L), to = c(2L, 3L), nodes = c("A", "B", "C"))
  expect_equal(e$from, c("A", "B"))
  expect_equal(e$to, c("B", "C"))
})

test_that("edge_vec() accepts a list `from`/`to` as a hyperedge column", {
  e <- edge_vec(
    from = list(c(1L, 2L)),
    to = 3L,
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_s3_class(e, "edge_vec")
  expect_length(e, 1L)
})

test_that("$.edge_vec resolves a hyperedge column to one node slice per edge", {
  e <- edge_vec(
    from = list(c(1L, 2L), 3L),
    to = c(3L, 1L),
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_equal(e$from, list(data.frame(label = c("A", "B")), data.frame(label = "C")))
  expect_equal(e$to, data.frame(label = c("C", "A")))
})

test_that("format.edge_vec() braces a hyperedge role with more than one node", {
  e <- edge_vec(
    from = list(c(1L, 2L)),
    to = 3L,
    nodes = data.frame(label = c("A", "B", "C"))
  )
  expect_equal(format(e), "[{A,B}]->[C]")
})

test_that("c.edge_vec() up-casts an ordinary `from`/`to` to a hyperedge column to combine with one", {
  e1 <- edge_vec(from = list(c(1L, 2L)), to = 1L, nodes = data.frame(label = c("A", "B")))
  e2 <- edge_vec(from = 1L, to = 2L, nodes = data.frame(label = c("X", "Y")))
  u <- c(e1, e2)

  expect_equal(format(u), c("[{A,B}]->[A]", "[X]->[Y]"))
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

test_that("format.edge_vec() renders `--` for undirected edges and `->` for directed edges", {
  e <- edge_vec(
    from = 1L,
    to = 2L,
    nodes = data.frame(label = c("A", "B")),
    directed = TRUE
  )
  expect_equal(format(e), "[A]->[B]")

  eu <- edge_vec(
    from = 1L,
    to = 2L,
    nodes = data.frame(label = c("A", "B")),
    directed = FALSE
  )
  expect_equal(format(eu), "[A]--[B]")
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

test_that("unique() and duplicated() compare edges by node values", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  g <- edge_vec(1L, 2L, w = 5, nodes = c("X", "Y"))
  expect_equal(duplicated(c(e, g, e)), c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(format(unique(c(e, g, e))), c("[A]->[B]", "[B]->[C]", "[X]->[Y]"))

  h <- edge_vec(list(1:2, 3L), list(3L, 1L), nodes = c("A", "B", "C"))
  expect_equal(duplicated(c(h, h)), c(FALSE, FALSE, TRUE, TRUE))
  ed <- edge_vec(1:2, 2:3, nodes = data.frame(id = 1:3, lab = c("A", "B", "C")))
  expect_equal(format(unique(c(ed, ed))), c("[1:A]->[2:B]", "[2:B]->[3:C]"))
})

test_that("[<- assigns edges of another graph as a disjoint union", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  g <- edge_vec(1L, 2L, w = 9, nodes = c("X", "Y"))
  x <- e
  x[1] <- g
  expect_equal(format(x), c("[X]->[Y]", "[B]->[C]"))
  expect_equal(x$w, c(9, 6))
  # Changed from A B C X Y: each graph's nodes now come in order of its
  # first row, the same as vctrs::vec_assign().
  expect_equal(attr(x, "nodes"), c("X", "Y", "A", "B", "C"))
  x[[2]] <- e[[1]]
  expect_equal(format(x), c("[X]->[Y]", "[A]->[B]"))
  expect_error(x[1] <- 1L, "Can only assign")

  h <- edge_vec(list(1:2), 3L, nodes = c("A", "B", "C"))
  x <- e
  x[2] <- h
  expect_equal(format(x), c("[A]->[B]", "[{A,B}]->[C]"))
})

test_that("[[ and as.list() give single-edge edge_vecs", {
  e <- edge_vec(1:2, 2:3, nodes = c("A", "B", "C"))
  expect_equal(e[[2]], e[2])
  expect_equal(as.list(e), list(e[1], e[2]))
  expect_error(e[[1:2]], "one element")
  expect_error(e[[3]], "out of bounds")
})

test_that("edge_vecs without node data combine as a disjoint union", {
  e <- edge_vec(1:2, 2:3)
  f <- edge_vec(1L, 2L)
  h <- edge_vec(list(1:2), list(4L))

  expect_equal(nrow(attr(e, "nodes")), 3L)
  expect_equal(format(c(e, f)), c("[1]->[2]", "[2]->[3]", "[4]->[5]"))
  expect_equal(format(c(e, h)), c("[1]->[2]", "[2]->[3]", "[{4,5}]->[7]"))
  expect_equal(nrow(attr(c(e, f), "nodes")), 5L)
  # Extra node rows beyond the edges' positions are kept.
  expect_equal(nrow(attr(edge_vec(1L, 2L, nodes = data.frame(row.names = 1:5)), "nodes")), 5L)
  # Slicing keeps every node.
  expect_equal(nrow(attr(e[1], "nodes")), 3L)
})

test_that("nodes() of an edge_vec without node data labels nodes by position", {
  e <- edge_vec(1:2, 2:3)
  expect_equal(format(nodes(e)), c("1", "2", "3"))
  expect_equal(format(edges(nodes(e))), format(e))
})

test_that(".DollarNames() completes from, to and edge attributes", {
  e <- edge_vec(1:2, 2:3, w = c(5, 6), nodes = c("A", "B", "C"))
  expect_equal(utils::.DollarNames(e, ""), c("from", "to", "w"))
  expect_equal(utils::.DollarNames(e, "^t"), "to")

  expect_equal(utils::.DollarNames(edge_vec(1:2, 2:3), ""), c("from", "to"))

  h <- edge_vec(list(1:2), 3L, nodes = c("A", "B", "C"))
  expect_equal(utils::.DollarNames(h, ""), c("from", "to"))
})

test_that("edge_vec can't be plotted without format()", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(y = 1:2)
  df$e <- edge_vec(1:2, 2:3, nodes = c("A", "B", "C"))
  p <- ggplot2::ggplot(df, ggplot2::aes(e, y)) + ggplot2::geom_point()
  expect_error(ggplot2::ggplot_build(p), "use format\\(\\)")
  p <- ggplot2::ggplot(df, ggplot2::aes(format(e), y)) + ggplot2::geom_point()
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), c("[A]->[B]", "[B]->[C]"))
})

test_that("is.na() and anyNA() give one value per edge", {
  e <- edge_vec(1:2, 2:3, w = c(1, NA), nodes = c("A", "B", "C"))
  expect_equal(is.na(e), c(FALSE, FALSE))
  expect_false(anyNA(e))
  # The Rust backend can't hold an ordinary edge with only one end missing.
  expect_error(edge_vec(1:2, c(2L, NA), nodes = c("A", "B", "C")), "only one of")

  e <- edge_vec(c(1L, NA), c(2L, NA), w = c(1, NA), nodes = c("A", "B"))
  expect_equal(is.na(e), c(FALSE, TRUE))
  expect_true(anyNA(e))
  expect_equal(is.na(edge_vec(nodes = "A")), logical())
})

test_that("an NA index gives a missing hyperedge, not an empty one", {
  h <- edge_vec(list(1:2, 3L), list(3L, 1L), nodes = c("A", "B", "C"))
  out <- h[c(1L, NA)]
  expect_equal(format(out), c("[{A,B}]->[C]", "[NA]->[NA]"))
  expect_equal(is.na(out), c(FALSE, TRUE))
  expect_true(anyNA(out))
  # A hyperedge with no nodes in a role isn't missing.
  expect_false(is.na(edge_vec(list(integer()), list(1L), nodes = "A")))
})

test_that("as.character.edge_vec() gives one label per edge", {
  e <- edge_vec(c(1L, 2L, 3L, 1L), c(2L, 3L, 1L, 2L), w = c(1, 2, 3, 1), nodes = c("A", "B", "C"))
  expect_equal(as.character(e), c("[A]->[B]", "[B]->[C]", "[C]->[A]", "[A]->[B]"))
  expect_equal(as.character(edge_vec(1:2, 2:3)), c("[1]->[2]", "[2]->[3]"))
})

test_that("edge_vec labels are used by pivot_wider(), write_csv() and str_c()", {
  e <- edge_vec(c(1L, 2L, 3L, 1L), c(2L, 3L, 1L, 2L), w = c(1, 2, 3, 1), nodes = c("A", "B", "C"))

  skip_if_not_installed("tidyr")
  wide <- tidyr::pivot_wider(
    tibble::tibble(e = e, v = 1:4),
    names_from = e, values_from = v, values_fn = sum
  )
  expect_named(wide, c("[A]->[B]", "[B]->[C]", "[C]->[A]"))
  expect_equal(wide[["[A]->[B]"]], 5L)

  skip_if_not_installed("readr")
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  readr::write_csv(tibble::tibble(e = e), f)
  expect_equal(readLines(f), c("e", "[A]->[B]", "[B]->[C]", "[C]->[A]", "[A]->[B]"))

  skip_if_not_installed("stringr")
  # stringi warns about coercing any list-backed object before it calls
  # as.character(), but the result is still the per-edge labels.
  expect_equal(
    suppressWarnings(stringr::str_c(e, "!")),
    c("[A]->[B]!", "[B]->[C]!", "[C]->[A]!", "[A]->[B]!")
  )
})

test_that("edge_vecs sort by node values, then attributes, with base and dplyr", {
  e <- edge_vec(c(2L, 1L, 1L, 3L, 1L), c(3L, 3L, 2L, 1L, 2L), w = c(1, 2, 3, 4, 0), nodes = c("A", "B", "C"))
  expect_equal(order(e), c(5L, 3L, 2L, 1L, 4L))
  expect_equal(rev(order(e)), c(4L, 1L, 2L, 3L, 5L))
  s <- sort(e)
  expect_s3_class(s, "edge_vec")
  expect_equal(format(s), c("[A]->[B]", "[A]->[B]", "[A]->[C]", "[B]->[C]", "[C]->[A]"))
  expect_equal(s$w, c(0, 3, 2, 1, 4))

  # Data frame node values sort column by column.
  d <- edge_vec(1:3, c(2L, 3L, 1L), nodes = data.frame(g = c("x", "x", "a"), k = 3:1))
  expect_equal(order(d), c(3L, 2L, 1L))
  # Without node data, by position.
  expect_equal(order(edge_vec(c(2L, 1L), c(1L, 2L))), c(2L, 1L))

  # Hyperedge node sets sort lexicographically, a prefix first.
  h <- edge_vec(list(1:2, 3L, 1L, 1:3), list(3L, 1L, 2L, 1L), nodes = c("A", "B", "C"))
  expect_equal(format(sort(h)), c("[A]->[B]", "[{A,B}]->[C]", "[{A,B,C}]->[A]", "[C]->[A]"))

  skip_if_not_installed("dplyr")
  expect_equal(order(dplyr::desc(e)), c(4L, 1L, 2L, 3L, 5L))
})

# -- graph identity (_dev/graph-identity.md) -------------------------------

test_that("c() of edge_vecs of the same graph shares its nodes", {
  e <- edge_vec(c(1L, 2L, 3L), c(2L, 3L, 1L), nodes = c("A", "B", "C"))
  u <- c(e, e)
  expect_identical(attr(u, "graph"), attr(e, "graph"))
  expect_equal(attr(u, "nodes"), c("A", "B", "C"))
  expect_equal(format(u), rep(format(e), 2))
  expect_equal(node_degree(c(e[1], e[2:3])), node_degree(e))
  x <- e
  x[2] <- e[2]
  expect_equal(x, e)
  expect_identical(attr(x, "graph"), attr(e, "graph"))

  # Different graphs are still a disjoint union, rows in order.
  f <- edge_vec(1L, 2L, nodes = c("X", "Y"))
  v <- c(e, f, e)
  expect_equal(attr(v, "nodes"), c("A", "B", "C", "X", "Y"))
  expect_equal(format(v), c(format(e), "[X]->[Y]", format(e)))
  expect_equal(n_nodes(nodes(v)), 5L)

  # Without node data too.
  e0 <- edge_vec(c(1L, 2L, 3L), c(2L, 3L, 1L))
  expect_equal(nrow(attr(c(e0, e0[2]), "nodes")), 3L)
  expect_equal(format(c(e0[2], e0)), c("[2]->[3]", "[1]->[2]", "[2]->[3]", "[3]->[1]"))
})

test_that("edge_vec duplicates are by graph and positions, not labels", {
  e <- edge_vec(c(1L, 2L, 1L), c(2L, 1L, 2L), nodes = c("A", "A"))
  # Same labels, different positions: different edges.
  expect_equal(duplicated(e), c(FALSE, FALSE, TRUE))
  expect_equal(format(unique(e)), c("[A]->[A]", "[A]->[A]"))
  f <- edge_vec(c(1L, 2L, 1L), c(2L, 1L, 2L), nodes = c("A", "A"))
  expect_equal(duplicated(c(e, f)), c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("an edge_vec with a missing edge works with nodes() and topology", {
  e <- edge_vec(c(1L, 2L), c(2L, 3L), w = 1:2, nodes = c("A", "B", "C"))
  x <- vctrs::vec_c(e, vctrs::vec_init(e, 1))
  expect_equal(is.na(x), c(FALSE, FALSE, TRUE))
  expect_identical(attr(x, "graph"), attr(e, "graph"))
  expect_equal(format(nodes(x)), c("A", "B", "C"))
  expect_equal(format(edges(nodes(x))), format(e))
  expect_equal(node_degree(x), c(1L, 2L, 1L))
  expect_equal(node_neighbors(x, 2, mode = "all"), c(1L, 3L))
  expect_equal(edge_is_loop(x), c(FALSE, FALSE, NA))
})

test_that("edge_vec errors on edge endpoints that aren't nodes", {
  for (bad in c(0L, -1L, 4L)) {
    expect_error(edge_vec(bad, 1L, nodes = c("a", "b", "c")), "between 1 and 3")
    expect_error(edge_vec(1L, bad, nodes = c("a", "b", "c")), "between 1 and 3")
    expect_error(new_edge_vec(bad, 1L, nodes = c("a", "b", "c")), "between 1 and 3")
  }
  # Without node data, the node count comes from the positions themselves.
  expect_error(edge_vec(0L, 1L), "positive")
  expect_error(edge_vec(-1L, 1L), "positive")
  expect_equal(n_nodes(edge_vec(1L, 5L)), 5L)
  # Missing at both ends is a missing edge, not an error.
  e <- edge_vec(c(1L, NA), c(2L, NA), nodes = c("a", "b"))
  expect_equal(is.na(e), c(FALSE, TRUE))
  expect_error(edge_vec(1L, NA_integer_, nodes = c("a", "b")), "only one")
})

test_that("== and != on an edge_vec compare by graph and positions", {
  e <- edges(node_vec(c("A", "B", NA), c(1L, 2L), c(2L, 3L)))
  expect_equal(e == e, c(TRUE, TRUE))
  expect_equal(e != e[2], c(TRUE, FALSE))
  expect_equal(e == e, vctrs::vec_equal(e, e))
  # A missing edge compares as NA.
  x <- c(e, vctrs::vec_init(e, 1))
  expect_equal(x == x, c(TRUE, TRUE, NA))
  # Edges of separately built graphs are never equal, whatever their labels.
  f <- edges(node_vec(c("A", "B", NA), c(1L, 2L), c(2L, 3L)))
  expect_equal(e == f, c(FALSE, FALSE))
  expect_equal(e != f, c(TRUE, TRUE))
})

test_that("an edge_vec errors on operators other than == and !=", {
  e <- edge_vec(1L, 2L, nodes = c("A", "B"))
  expect_snapshot(error = TRUE, {
    e < e
    e + 1
    !e
  })
})

test_that("anyDuplicated.edge_vec() agrees with duplicated()", {
  e <- edges(node_vec(c("A", "B", NA), c(1L, 2L), c(2L, 3L)))
  expect_equal(anyDuplicated(e), 0L)
  expect_equal(anyDuplicated(c(e, e)), 3L)
  expect_equal(anyDuplicated(c(e, e), fromLast = TRUE), 2L)
  h <- edge_vec(list(1:2, 2L, 1:2), list(3L, 1L, 3L), nodes = c("A", "B", "C"))
  expect_equal(anyDuplicated(h), 3L)
})

test_that("match() and %in% on edge_vecs agree with vec_match()", {
  skip_if(getRversion() < "4.3.0", "match() only uses mtfrm() from R 4.3")
  # Edge attributes match exactly: 0.1 + 0.2 isn't 0.3, but -0 is 0.
  w <- edge_vec(
    c(1L, 1L, 2L, 1L, 1L), c(2L, 2L, 3L, 2L, 2L),
    w = c(0.1 + 0.2, 0.3, NA, -0, 0), nodes = c("A", "B", "C")
  )
  table <- w[c(2, 1, 3, 5)]
  expect_equal(match(w, table), vctrs::vec_match(w, table))
  expect_equal(match(w, table), c(2L, 1L, 3L, 4L, 4L))
  expect_equal(w %in% table[1:2], vctrs::vec_in(w, table[1:2]))

  # Edges of a separately built graph never match.
  v <- edge_vec(1L, 2L, w = 0.3, nodes = c("A", "B", "C"))
  expect_equal(match(v, w), vctrs::vec_match(v, w))
  expect_equal(match(v, w), NA_integer_)

  h <- edge_vec(list(1:2, 2L, 1:2), list(3L, 1L, 3L), nodes = c("A", "B", "C"))
  expect_equal(match(h, h), vctrs::vec_match(h, h))
  expect_equal(match(h, h), c(1L, 2L, 1L))
})

test_that("edge_vec() errors on an edge attribute named like a misspelt option", {
  expect_snapshot(error = TRUE, {
    edge_vec(1L, 2L, node = c("a", "b"))
    edge_vec(1L, 2L, directd = FALSE)
    new_edge_vec(1L, 2L, dir = FALSE)
  })
  expect_error(edge_vec(1L, 2L, Nodes = 1), class = "rlang_error")
  expect_error(edge_vec(1L, 2L, nodse = 1), class = "rlang_error")
  expect_error(edge_vec(1L, 2L, undirected = TRUE), class = "rlang_error")
})

test_that("edge_vec() accepts common edge attribute names", {
  e <- edge_vec(
    1L, 2L,
    weight = 1, type = "a", label = "x", name = "n", id = 1L, group = "g",
    notes = "", direction = "out",
    nodes = c("A", "B"), directed = FALSE
  )
  expect_equal(e$weight, 1)
  expect_equal(e$group, "g")
  expect_equal(e$direction, "out")
  expect_false(attr(e, "directed"))
})
