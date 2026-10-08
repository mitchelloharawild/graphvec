test_that("node_vec() returns a node_vec object", {
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  expect_s3_class(g, "node_vec")
})

test_that("node_vec() preserves the length of x", {
  g <- node_vec(
    x = factor(c("A", "B", "C", "D")),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  expect_length(g, 4L)
})

test_that("node_vec() records the edges between the given nodes", {
  # Rewritten: the non-hyperedge case no longer stores from/to positions
  # directly in attr(x, "edges") (_dev/RUST_BACKEND.md moves topology into
  # the Rust GraphBackend) -- check the same logical property (which edges
  # exist) via the public edges()/format() surface instead.
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  expect_equal(format(edges(g)), c("[A]->[B]", "[B]->[C]"))
})

test_that("node_vec() accepts an empty vector", {
  g <- node_vec()
  expect_s3_class(g, "node_vec")
  expect_length(g, 0L)
})

test_that("node_vec() accepts a list `from`/`to` as a hyperedge column", {
  # A single hyperedge: "from" nodes 1 and 2 both feed into node 3.
  g <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  expect_s3_class(g, "node_vec")
  expect_equal(attr(g, "edges")$from, list(c(1L, 2L)), ignore_attr = TRUE)
})

test_that("node_vec() rejects a `from`/`to` list containing non-integer elements", {
  expect_error(node_vec(x = c("A", "B", "C"), from = list("A"), to = 2L))
})

test_that("`[.node_vec` drops a hyperedge losing any one of its members", {
  # A -> C hyperedge from {A, B}; slicing out B should drop the whole edge.
  g <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  m <- g[c(1, 3)]
  expect_length(attr(m, "edges")$from, 0L)
})

test_that("`[.node_vec` makes a copy of a hyperedge graph per repeated member", {
  g <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  # The first A is in the copy with B and C, so keeps the hyperedge; the
  # second is a copy of its own, with no edges.
  m <- g[c(1, 1, 2, 3)]
  expect_equal(attr(m, "edges")$from, list(c(1L, 3L)), ignore_attr = TRUE)
  expect_equal(attr(m, "edges")$to, 4L)
  expect_equal(g[c(1:3, 1:3)], c(g, g))
})

test_that("c.node_vec() up-casts an ordinary `from`/`to` to a hyperedge column to combine with one", {
  g1 <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  g2 <- node_vec(x = c("X", "Y"), from = 1L, to = 2L)
  u <- c(g1, g2)
  expect_equal(
    attr(u, "edges")$from,
    list(c(1L, 2L), 4L),
    ignore_attr = TRUE
  )
  expect_equal(attr(u, "edges")$to, c(3L, 5L))
})

test_that("node_vec() accepts a data frame of node attributes, sized by row count", {
  g <- node_vec(
    x = data.frame(name = c("A", "B", "C"), size = c(10, 4, 7)),
    from = 1L,
    to = 2L
  )
  expect_s3_class(g, "node_vec")
  expect_length(g, 3L)
  expect_equal(node_vec_data(g), data.frame(name = c("A", "B", "C"), size = c(10, 4, 7)))
})

test_that("data-frame-valued node_vec slices as an induced subgraph, same as any other", {
  g <- node_vec(
    x = data.frame(name = c("A", "B", "C", "D")),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L)
  )
  m <- g[2:3]
  expect_length(m, 2L)
  expect_equal(node_vec_data(m), data.frame(name = c("B", "C")))
  # Rewritten (see the analogous rewrite above): check the surviving edge via
  # edges()/format() rather than attr(x, "edges")'s internal positions.
  expect_equal(format(edges(m)), "[B]->[C]")
})

test_that("node_vec() accepts named edge attributes via ...", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L),
    weight = c(1, 2, 5)
  )
  expect_equal(attr(g, "edges")$weight, c(1, 2, 5))
})

test_that("node_vec() rejects unnamed edge attributes", {
  expect_error(
    node_vec(x = c("A", "B"), from = 1L, to = 2L, 5),
    "must be named"
  )
})

test_that("`[.node_vec` carries edge attributes through the induced-subgraph remap", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L),
    weight = c(1, 2, 5)
  )
  m <- g[2:3]
  # Rewritten: a slice now keeps its parent's whole graph (and attribute
  # table) and computes its induced edges on read, so check via edges().
  expect_equal(edges(m)$weight, 2)
})

test_that("`[.node_vec` copies edge attributes when a node is repeated", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L, weight = 42)
  # Rewritten (see above): check via edges(), not the shared attribute table.
  expect_equal(edges(g[c(1, 1, 2)])$weight, 42)
  expect_equal(edges(g[c(1, 2, 1, 2)])$weight, c(42, 42))
})

test_that("format.node_vec() formats the underlying vector", {
  g <- node_vec(x = factor(c("A", "B")), from = 1L, to = 2L)

  expect_equal(format(g), c("A", "B"))
})

test_that("format.node_vec() preserves wrapped agg_vec formatting", {
  g <- node_vec(
    x = agg_vec(c(NA, "A", "B"), aggregated = c(TRUE, FALSE, FALSE)),
    from = 1L,
    to = 2L
  )

  expect_equal(format(g), c("<aggregated>", "A", "B"))
})

test_that("type_sum.node_vec() abbreviates the node data type", {
  skip_if_not_installed("pillar")
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  expect_equal(pillar::type_sum(g), "N[chr]")

  g <- node_vec(x = factor(c("A", "B")), from = 1L, to = 2L)
  expect_equal(pillar::type_sum(g), "N[fct]")
})

test_that("new_node_vec() is a low-level constructor for node_vec", {
  g <- new_node_vec(
    x = c("A", "B"),
    edges = data.frame(from = 1L, to = 2L)
  )
  expect_s3_class(g, "node_vec")
})

test_that("`[.node_vec` remaps surviving edges to the new positions", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L)
  )
  m <- g[2:3]
  expect_length(m, 2L)
  expect_equal(format(m), c("B", "C"))
  # Rewritten (see above): check via edges()/format(), not internal positions.
  expect_equal(format(edges(m)), "[B]->[C]")
})

test_that("`[.node_vec` drops edges that lose an endpoint", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L)
  )
  m <- g[c(1, 4)]
  expect_length(m, 2L)
  # Rewritten: attr(m, "edges")$to was NULL either way once "to" stopped
  # being a column of the ordinary case's edges attribute, which made the
  # original assertion (expect_length(..., 0L)) pass vacuously regardless of
  # whether the edge actually survived -- check edge count via edges()
  # instead, which does exercise the drop.
  expect_length(edges(m), 0L)
})

test_that("`[.node_vec` makes a separate copy of the graph per repeat, as c() does", {
  g <- node_vec(x = c("A", "B", "C"), from = 1:2, to = 2:3)
  # The k-th occurrence of each node is in the k-th copy: here the first A
  # with B and C, and the second A on its own.
  m <- g[c(1, 1, 2, 3)]
  expect_length(m, 4L)
  expect_equal(format(m), c("A", "A", "B", "C"))
  expect_equal(edge_pairs(m), c("1->3", "3->4"))
  expect_equal(m, c(g, g[1])[c(1, 4, 2, 3)])
  # Copies equal the node they copy.
  expect_equal(m == g[c(1, 1, 2, 3)], rep(TRUE, 4))
  expect_equal(duplicated(m), c(FALSE, TRUE, FALSE, FALSE))
  # Repeating every node is c(), the same as rep().
  expect_equal(g[c(1:3, 1:3)], c(g, g))
  expect_equal(n_edges(g[c(1:3, 1:3)]), 4L)
  expect_equal(g[c(1, 2, 3, 1, 2, 3)], rep(g, 2))
  expect_equal(g[c(1, 1, 2, 2, 3, 3)], rep(g, each = 2))
  # A missing index is a missing node, never a copy.
  expect_equal(edge_pairs(g[c(1, NA, 2, NA)]), "1->3")

  # Data-frame node values likewise.
  d <- node_vec(data.frame(id = 1:2, lab = c("a", "b")), 1L, 2L)
  expect_equal(d[c(1, 2, 1, 2)], c(d, d))
  expect_equal(format(d[c(2, 1, 2)]), c("2:b", "1:a", "2:b"))
  expect_equal(edge_pairs(d[c(2, 1, 2)]), "2->1")
})

test_that("`[.node_vec` supports negative and logical indices", {
  g <- node_vec(x = c("A", "B", "C"), from = 1L, to = 2L)
  expect_equal(format(g[-1]), c("B", "C"))
  expect_equal(format(g[c(TRUE, TRUE, FALSE)]), c("A", "B"))
})

test_that("node_vec() defaults to directed = TRUE and stores it as an attribute", {
  g <- node_vec(x = c("A", "B"))
  expect_true(attr(g, "directed"))

  gu <- node_vec(x = c("A", "B"), directed = FALSE)
  expect_false(attr(gu, "directed"))
})

test_that("node_vec() validates directed as a single non-NA logical", {
  expect_error(node_vec(x = c("A"), directed = NA))
  expect_error(node_vec(x = c("A"), directed = c(TRUE, FALSE)))
  expect_error(node_vec(x = c("A"), directed = "TRUE"))
})

test_that("directed survives `[` on a node_vec", {
  gu <- node_vec(
    x = c("A", "B", "C"),
    from = 1L,
    to = 2L,
    directed = FALSE
  )
  expect_false(attr(gu[2:3], "directed"))
})

test_that("node_vec slicing works as a data frame column (e.g. under dplyr)", {
  skip_if_not_installed("dplyr")
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L)
  )
  df <- data.frame(id = 1:4)
  df$g <- g
  filtered <- dplyr::filter(df, id %in% c(2, 3))
  expect_equal(format(filtered$g), c("B", "C"))
  # Rewritten (see above): check via edges()/format(), not internal positions.
  expect_equal(format(edges(filtered$g)), "[B]->[C]")
})

test_that("node_vec() layers its class onto x rather than boxing it, so x's own methods still work", {
  g <- node_vec(x = factor(c("lo", "hi"), levels = c("lo", "hi")), from = 1L, to = 2L)
  expect_equal(levels(g), c("lo", "hi"))
  expect_equal(class(g), c("node_vec", "factor"))
})

test_that("length() uses x's row count, not ncol(), for a data-frame-backed node_vec", {
  g <- node_vec(x = data.frame(name = c("A", "B", "C")), from = 1L, to = 2L)
  expect_length(g, 3L)
})

test_that("node_vec() excludes \"data.frame\" from a data-frame-backed x's layered class, but `$`/slicing/length still work", {
  # is.data.frame(g) must stay FALSE even when x is a data frame -- pillar's
  # tibble-column renderer checks it directly (not via S3 dispatch) to decide
  # whether to treat a column as a *nested* tibble, which used to crash on
  # node_vec's edges/directed attributes (_dev/tidy.md §1).
  g <- node_vec(
    x = data.frame(name = c("A", "B", "C"), size = c(10, 4, 7)),
    from = 1L, to = 2L, weight = 5
  )
  expect_false(is.data.frame(g))
  expect_false("data.frame" %in% class(g))
  expect_equal(g$name, c("A", "B", "C"))
  expect_length(g, 3L)
  expect_equal(format(g[2:3]), c("B:4", "C:7"))
})

test_that("a data-frame-backed node_vec can be embedded as a tibble column", {
  # A data-frame-backed node_vec is list-typed, so vctrs::obj_is_vector()
  # (which tibble::tibble() requires) only accepts it through the
  # registered vec_proxy.node_vec().
  skip_if_not_installed("tibble")
  g <- node_vec(
    x = data.frame(name = c("A", "B", "C"), size = c(10, 4, 7)),
    from = 1L, to = 2L
  )
  d <- tibble::tibble(id = 1:3, g = g)
  expect_equal(format(d$g), c("A:10", "B:4", "C:7"))
  expect_equal(node_vec_data(vctrs::vec_slice(d$g, 2:1)), data.frame(name = c("B", "A"), size = c(4, 10)))
})

test_that("sort(), rev(), head() route through `[` and inherit its induced-subgraph remap", {
  g <- node_vec(x = c("C", "A", "B"), from = c(1L, 2L), to = c(2L, 3L))

  s <- sort(g)
  expect_equal(format(s), c("A", "B", "C"))
  # Rewritten (see above): reordering nodes never changes which *values* an
  # edge connects, so the same logical edges (checked via edges()/format(),
  # not internal positions) should survive sort() unchanged.
  expect_equal(format(edges(s)), c("[C]->[A]", "[A]->[B]"))

  r <- rev(g)
  expect_equal(format(r), c("B", "A", "C"))

  h <- head(g, 2)
  expect_equal(format(h), c("C", "A"))
  expect_equal(format(edges(h)), "[C]->[A]")
})

test_that("unique.node_vec() drops repeats of a node and their incident edges, via `[`", {
  g <- node_vec(x = c("A", "A", "B"), from = c(1L, 2L), to = c(2L, 3L))
  # Two nodes with the same label are still different nodes.
  expect_equal(unique(g), g)
  # Rewritten: was value-based ("A", "A" collapsed). Repeating node 1 makes
  # a second copy of it (with no edges of its own here); dropping the copy
  # gives back `g`.
  r <- g[c(1, 2, 1, 3)]
  expect_equal(n_edges(r), 2L)
  u <- unique(r)
  expect_equal(u, g)
  expect_equal(n_edges(u), 2L)
})

test_that("== and != on node_vecs compare as vec_equal() does", {
  n <- node_vec(c("A", "B", "A"), 1:2, 2:3)
  # Nodes with the same label are still different nodes.
  expect_equal(n == n[3:1], c(FALSE, TRUE, FALSE))
  expect_equal(n != n[3:1], c(TRUE, FALSE, TRUE))
  expect_equal(n == n[3:1], vctrs::vec_equal(n, n[3:1]))
  expect_equal(n == n[1], c(TRUE, FALSE, FALSE))
  expect_error(n == n[1:2], "recycle")
  # A copy equals the node it copies, as for duplicated().
  expect_equal(c(n, n)[4:6] == n, c(TRUE, TRUE, TRUE))
  # Nodes of separately built graphs are never equal.
  expect_equal(n == node_vec(c("A", "B", "A"), 1:2, 2:3), c(FALSE, FALSE, FALSE))
  # A relabelled node no longer equals the original.
  r <- n
  r[2] <- "Z"
  expect_equal(r == n, c(TRUE, FALSE, TRUE))
  # A missing node compares as NA.
  x <- c(n, vctrs::vec_init(n, 1))
  expect_equal(x == x, c(TRUE, TRUE, TRUE, NA))

  # A node is never equal to a plain value, and nodes have no order or
  # arithmetic: compare their values for that.
  expect_snapshot(error = TRUE, {
    n == "A"
    "A" != n
    n < n
    n + 1
    !n
  })
  expect_equal(node_values(n) == "A", c(TRUE, FALSE, TRUE))

  # Data-frame values compare row-wise.
  d <- node_vec(data.frame(id = 1:2, lab = c("a", "b")), 1L, 2L)
  expect_equal(d == d, c(TRUE, TRUE))
  expect_equal(d == d[2:1], c(FALSE, FALSE))

  # Hyperedge node_vecs have no graph identity and compare by value.
  h <- node_vec(c("A", "B", "A"), from = list(1:2), to = list(3L))
  expect_equal(h == h[3:1], c(TRUE, TRUE, TRUE))
  expect_equal(h == h[3:1], vctrs::vec_equal(h, h[3:1]))
})

test_that("match() and %in% on node_vecs agree with vec_match()", {
  skip_if(getRversion() < "4.3.0", "match() only uses mtfrm() from R 4.3")
  n <- node_vec(c("A", "B", "A", NA), 1:2, 2:3)
  # Nodes match by identity and value, not label.
  expect_equal(match(n, n[4:1]), vctrs::vec_match(n, n[4:1]))
  expect_equal(match(n, n[4:1]), 4:1)
  expect_equal(n %in% n[3], vctrs::vec_in(n, n[3]))
  expect_equal(n %in% n[3], c(FALSE, FALSE, TRUE, FALSE))
  # Copies match the node they copy; a node of a separately built graph or
  # a relabelled node doesn't.
  expect_equal(match(c(n, n), n), rep(1:4, 2))
  expect_equal(match(node_vec(c("A", "B", "A", NA), 1:2, 2:3), n), rep(NA_integer_, 4))
  r <- n
  r[2] <- "Z"
  expect_equal(match(r, n), c(1L, NA, 3L, 4L))
  # A missing node matches only missing nodes.
  x <- c(n, vctrs::vec_init(n, 1))
  expect_equal(match(x, x[5:1]), vctrs::vec_match(x, x[5:1]))
  expect_equal(match(x[5], n), NA_integer_)

  d <- node_vec(data.frame(id = 1:2, lab = c("a", "b")), 1L, 2L)
  expect_equal(match(d, d[2:1]), c(2L, 1L))

  # Hyperedge node_vecs match by value.
  h <- node_vec(c("A", "B", "A"), from = list(1:2), to = list(3L))
  expect_equal(match(h, h), vctrs::vec_match(h, h))
  expect_equal(match(h, h), c(1L, 2L, 1L))

  # A node never matches a plain value; node_values() matches by value.
  expect_equal(match(n, "A"), rep(NA_integer_, 4))
  expect_equal(n %in% c("A", "B"), rep(FALSE, 4))
  expect_equal(match("A", n), NA_integer_)
  expect_equal(node_values(n) %in% c("A", "B"), c(TRUE, TRUE, TRUE, FALSE))
  expect_error(vctrs::vec_match(n, "A"), "combine")
})

test_that("c.node_vec() is a disjoint union: values concatenate, second graph's edges are offset", {
  g1 <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  g2 <- node_vec(x = c("X", "Y"), from = 1L, to = 2L)
  u <- c(g1, g2)

  expect_equal(format(u), c("A", "B", "X", "Y"))
  # Rewritten (see above): check via edges()/format(), not internal positions.
  expect_equal(format(edges(u)), c("[A]->[B]", "[X]->[Y]"))
})

test_that("c.node_vec() rejects combining with a non-node_vec or a mismatched `directed`", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  expect_error(c(g, 1:2), "node_vec")
  expect_error(c(g, node_vec(x = "Z", directed = FALSE)), "directed")
})

test_that("c.node_vec() pads a missing edge attribute with NA when combining node_vec objects", {
  g1 <- node_vec(x = c("A", "B"), from = 1L, to = 2L, weight = 5)
  g2 <- node_vec(x = c("X", "Y"), from = 1L, to = 2L)
  u <- c(g1, g2)

  expect_equal(attr(u, "edges")$weight, c(5, NA))
})

test_that("c.node_vec() combines a graph with edge attributes and one with no edges", {
  g <- node_vec(c("A", "B", "C"), c(1L, 2L), c(2L, 3L), weight = c(1, 2))
  u <- c(g, node_vec("Z"))
  expect_equal(format(u), c("A", "B", "C", "Z"))
  expect_equal(attr(u, "edges")$weight, c(1, 2))
  expect_equal(attr(c(node_vec("Z"), g), "edges")$weight, c(1, 2))
  expect_equal(vctrs::vec_c(g, node_vec("Z")), u)

  skip_if_not_installed("dplyr")
  b <- dplyr::bind_rows(tibble::tibble(g = g), tibble::tibble(g = node_vec("Z")))
  expect_equal(b$g, u)
})

test_that("rep.node_vec() makes disjoint copies of the graph, as c() does", {
  g <- node_vec(x = c("A", "B", "A"), from = 1:2, to = 2:3, weight = c(1, 2))
  r <- rep(g, 2)
  expect_equal(format(r), rep(c("A", "B", "A"), 2))
  # Each copy keeps only its own edges (4, not 8 cloned per combination of
  # repeats), with their attributes, and every node equals the node it
  # copies.
  expect_equal(r, c(g, g))
  expect_equal(format(edges(r)), rep(c("[A]->[B]", "[B]->[A]"), 2))
  expect_equal(edges(r)$weight, c(1, 2, 1, 2))
  expect_equal(vctrs::vec_equal(r, c(g, g)), rep(TRUE, 6))
  expect_equal(duplicated(r), rep(c(FALSE, TRUE), each = 3))

  # The k-th repeat of each node goes into the k-th copy.
  r <- rep(g, each = 2)
  expect_equal(format(r), rep(c("A", "B", "A"), each = 2))
  expect_equal(r, c(g, g)[c(1, 4, 2, 5, 3, 6)])
  expect_equal(format(edges(r)), rep(c("[A]->[B]", "[B]->[A]"), 2))
  expect_equal(rep(g, times = c(2, 1, 2)), c(g, g[c(1, 3)])[c(1, 4, 2, 3, 5)])
  expect_equal(rep(g, length.out = 5), c(g, g[1:2]))
  expect_equal(format(edges(rep(g, length.out = 5))), c("[A]->[B]", "[B]->[A]", "[A]->[B]"))

  # No repeats: just a slice.
  expect_equal(rep(g, 1), g)
  expect_equal(rep(g, times = c(1, 0, 1)), g[c(1, 3)])
  expect_length(rep(g, 0), 0L)

  # Hyperedge node_vecs, which have no graph identity, likewise.
  h <- node_vec(c("A", "B", "C"), from = list(1:2), to = list(3L))
  expect_equal(rep(h, 2), c(h, h))
  expect_equal(format(edges(rep(h, 2))), rep("[{A,B}]->[C]", 2))
})

test_that("append() works on a node_vec via length()/c()/`[` without a bespoke method", {
  g1 <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  g2 <- node_vec(x = "Z")
  a <- append(g1, g2)
  expect_equal(format(a), c("A", "B", "Z"))
})

test_that("as.character.node_vec() delegates to x's own value", {
  g <- node_vec(x = factor(c("A", "B")), from = 1L, to = 2L)
  expect_equal(as.character(g), c("A", "B"))
})

test_that("as.character.node_vec() gives one label per node for data frame values", {
  g <- node_vec(data.frame(id = 1:2, lab = c("a", "b")))
  expect_equal(as.character(g), c("1:a", "2:b"))
})

test_that("a data-frame node_vec column writes one label per node with readr", {
  skip_if_not_installed("readr")
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  readr::write_csv(tibble::tibble(n = node_vec(data.frame(id = 1:2, lab = c("a", "b")))), f)
  expect_equal(readLines(f), c("n", "1:a", "2:b"))
})

test_that("order() sorts a node_vec by node value, not position", {
  g <- node_vec(x = c(3, 1, 2), from = 1L, to = 2L)
  expect_equal(order(g), c(2L, 3L, 1L))
})

test_that("a data-frame node_vec sorts row-wise by value with base and dplyr", {
  g <- node_vec(data.frame(id = c(2L, 1L, 2L), lab = c("b", "z", "a")), from = 1L, to = 2L)
  expect_equal(order(g), c(2L, 3L, 1L))
  expect_equal(rev(order(g)), c(1L, 3L, 2L))
  s <- sort(g)
  expect_s3_class(s, "node_vec")
  expect_equal(format(s), c("1:z", "2:a", "2:b"))
  # The edge still joins 2:b to 1:z, now at positions 3 and 1.
  expect_equal(edge_pairs(s), "3->1")

  skip_if_not_installed("dplyr")
  expect_equal(order(dplyr::desc(g)), c(1L, 3L, 2L))
})

test_that("print.node_vec() shows a header and the formatted values, not raw attributes", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  expect_output(print(g), "<node_vec[2]>", fixed = TRUE)
  expect_output(print(g), "[1] A B", fixed = TRUE)
  expect_false(grepl("attr\\(,", paste(capture.output(print(g)), collapse = "\n")))
})

test_that("[<- relabels nodes with plain values", {
  n <- node_vec(c("A", "B", "C"), from = 1:2, to = 2:3)
  n[1] <- "Z"
  expect_equal(format(n), c("Z", "B", "C"))
  expect_equal(format(edges(n)), c("[Z]->[B]", "[B]->[C]"))

  nd <- node_vec(data.frame(id = 1:2, lab = c("A", "B")), from = 1L, to = 2L)
  nd[2] <- data.frame(id = 9L, lab = "Z")
  expect_equal(format(nd), c("1:A", "9:Z"))
  expect_length(edges(nd), 1L)
})

test_that("[<- with a node_vec swaps in its nodes and their edges", {
  n <- node_vec(c("A", "B", "C"), from = 1:2, to = 2:3)
  m <- node_vec(c("X", "Y"), from = 1L, to = 2L)
  x <- n
  x[2:3] <- m
  expect_equal(format(x), c("A", "X", "Y"))
  expect_equal(format(edges(x)), "[X]->[Y]")
  expect_equal(x, vctrs::vec_assign(n, 2:3, m))

  nd <- node_vec(data.frame(id = 1:3), from = 1:2, to = 2:3)
  df <- tibble::tibble(x = nd)
  df$x[2] <- nd[3]
  expect_equal(format(df$x), c("1", "3", "3"))
})

test_that("element access on a data-frame node_vec works on nodes, not columns", {
  nd <- node_vec(data.frame(id = c(1L, 1L, 2L), lab = c("A", "A", "B")), 1:2, 2:3)
  expect_equal(nd[[3]], nd[3])
  expect_equal(node_vec_data(nd[[3]]), data.frame(id = 2L, lab = "B"))
  expect_error(nd[[4]], "out of bounds")
  expect_equal(as.list(nd), list(nd[1], nd[2], nd[3]))
  expect_equal(vapply(nd, format, character(1)), c("1:A", "1:A", "2:B"))
  # Rows 1 and 2 have the same values but are different nodes.
  expect_equal(duplicated(nd), c(FALSE, FALSE, FALSE))
  expect_equal(duplicated(nd[c(1, 3, 1)]), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(nd[c(1, 1, 3)]), 2L)
  expect_equal(format(unique(nd[c(1, 1, 3)])), c("1:A", "2:B"))

  n1 <- node_vec(data.frame(id = c(5L, 6L, 5L)), 1L, 2L)
  expect_equal(format(n1[[3]]), "5")
  expect_equal(duplicated(n1), c(FALSE, FALSE, FALSE))
  expect_equal(duplicated(n1[c(1, 2, 1)]), c(FALSE, FALSE, TRUE))
})

test_that("[[ and as.list() give single-node node_vecs", {
  n <- node_vec(c(a = "A", b = "B", c = "A"), 1:2, 2:3)
  expect_equal(n[[2]], n[2])
  expect_s3_class(n[[2]], "node_vec")
  expect_true(node_values(n[[2]]) == "B")
  expect_equal(n[["b"]], n[2])
  expect_equal(as.list(n), list(a = n[1], b = n[2], c = n[3]))
  expect_equal(vapply(n, format, character(1)), c(a = "A", b = "B", c = "A"))
  expect_error(n[[1:2]], "one element")
  expect_error(n[[NA]], "one element")
  expect_error(n[[4]], "out of bounds")
  expect_error(n[[0]], "out of bounds")
  expect_error(n[["z"]], "out of bounds")
  expect_equal(duplicated(n), c(FALSE, FALSE, FALSE))
  expect_equal(duplicated(n[c(1, 2, 1)]), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(n[c(1, 2, 1)]), 3L)
})

test_that("purrr::map() works element-wise on node_vecs", {
  skip_if_not_installed("purrr")
  nd <- node_vec(data.frame(id = 1:2, lab = c("A", "B")), 1L, 2L)
  expect_equal(purrr::map_chr(nd, format), c("1:A", "2:B"))
  expect_equal(format(purrr::map_vec(nd, identity)), c("1:A", "2:B"))
  n <- node_vec(c("A", "B", "C"), 1:2, 2:3)
  expect_equal(purrr::map_chr(n, format), c("A", "B", "C"))
  # Each element is a slice of `n`'s graph at a different position, so
  # putting them back together restores every edge (it used to be a
  # disjoint union of edgeless singletons).
  m <- purrr::map_vec(n, identity)
  expect_s3_class(m, "node_vec")
  expect_equal(format(m), c("A", "B", "C"))
  expect_equal(edge_pairs(m), edge_pairs(n))
  expect_equal(format(purrr::modify(n, identity)), c("A", "B", "C"))
})

test_that("[[<- assigns a single node, like [<-", {
  n <- node_vec(c("A", "B", "C"), 1:2, 2:3)
  x <- n
  x[[2]] <- "Z"
  expect_equal(x, {y <- n; y[2] <- "Z"; y})
  expect_equal(edge_pairs(x), edge_pairs(n))
  x <- n
  x[[2]] <- n[[3]]
  expect_equal(x, {y <- n; y[2] <- n[3]; y})
  expect_error(x[[2]] <- c("a", "b"), "single node")
  expect_error(x[[1:2]] <- "a", "one element")

  nd <- node_vec(data.frame(id = 1:3, lab = c("a", "b", "c")), 1:2, 2:3)
  x <- nd
  x[[2]] <- data.frame(id = 9L, lab = "z")
  expect_equal(format(x), c("1:a", "9:z", "3:c"))
  expect_equal(edge_pairs(x), edge_pairs(nd))
})

test_that("a node_vec's ggplot2 scale type is \"node\", then its values'", {
  skip_if_not_installed("ggplot2")
  st <- function(x) ggplot2::scale_type(node_vec(x))
  expect_equal(st(c("A", "B")), c("node", "discrete"))
  expect_equal(st(factor(c("A", "B"))), c("node", "discrete"))
  expect_equal(st(c(TRUE, FALSE)), c("node", "discrete"))
  # Nodes have no arithmetic for a continuous scale, so "discrete" takes the
  # place of "continuous", after any type the values name first; a data
  # frame of values has no scale type of its own.
  expect_equal(st(as.Date("2020-01-01")), c("node", "date", "discrete"))
  expect_equal(st(as.POSIXct("2020-01-01", tz = "UTC")), c("node", "datetime", "discrete"))
  expect_equal(st(c(1, 2)), c("node", "discrete"))
  expect_equal(st(data.frame(lab = c("A", "B"))), c("node", "discrete"))
})

test_that("character, factor and logical node_vecs plot on any aesthetic", {
  skip_if_not_installed("ggplot2")
  for (v in list(c("B", "A", "C"), factor(c("B", "A", "C")), c(TRUE, FALSE, TRUE))) {
    df <- data.frame(y = 1:3)
    df$v <- node_vec(v, 1:2, 2:3)
    p <- ggplot2::ggplot(df, ggplot2::aes(v, y, colour = v)) + ggplot2::geom_point()
    b <- expect_silent(ggplot2::ggplot_build(p))
    labels <- sort(unique(as.character(v)))
    expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleDiscretePosition")
    expect_equal(b$layout$panel_params[[1]]$x$get_labels(), labels)
    expect_equal(as.numeric(b$data[[1]]$x), match(as.character(v), labels))
    expect_equal(b$plot$scales$get_scales("colour")$get_labels(), labels)
  }

  # Repeated labels share a level, but ggplot2 groups by node, so the two
  # "A"s are separate groups.
  df <- data.frame(y = 1:3)
  df$n <- node_vec(c("B", "A", "A"), 1:2, 2:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(y, y, colour = n, shape = n)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_equal(b$plot$scales$get_scales("shape")$get_labels(), c("A", "B"))
  expect_equal(b$data[[1]]$colour, b$plot$scales$get_scales("colour")$map(c("B", "A", "A")))
  if (getRversion() >= "4.3.0") {
    expect_equal(b$data[[1]]$group, c(3L, 1L, 2L), ignore_attr = TRUE)
  }
})

test_that("date and date-time node_vecs plot with date scales", {
  skip_if_not_installed("ggplot2")
  dates <- as.Date("2020-01-01") + c(2, 0, 1)
  times <- as.POSIXct("2020-01-01", tz = "UTC") + c(2, 0, 1) * 3600
  for (v in list(dates, times)) {
    df <- data.frame(y = 1:3)
    df$v <- node_vec(v, 1:2, 2:3)
    p <- ggplot2::ggplot(df, ggplot2::aes(v, y, colour = v)) + ggplot2::geom_point()
    b <- expect_silent(ggplot2::ggplot_build(p))
    expect_equal(b$data[[1]]$x, as.numeric(v))
    expect_equal(b$data[[1]]$y, 1:3)
    expect_length(unique(b$data[[1]]$colour), 3L)
  }
  expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleContinuousDatetime")
})

test_that("numeric and data-frame node_vecs plot as discrete colours, not positions", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(y = 1:3)
  df$v <- node_vec(c(20.5, 10, 30), 1:2, 2:3)
  df$d <- node_vec(data.frame(id = 3:1, lab = c("C", "B", "A")), 1:2, 2:3)

  p <- ggplot2::ggplot(df, ggplot2::aes(y, y, colour = v, fill = d)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_equal(b$plot$scales$get_scales("colour")$get_labels(), c("10", "20.5", "30"))
  expect_equal(b$data[[1]]$colour, b$plot$scales$get_scales("colour")$map(c("20.5", "10", "30")))
  expect_equal(b$plot$scales$get_scales("fill")$get_labels(), c("1:A", "2:B", "3:C"))
  expect_equal(b$data[[1]]$fill, b$plot$scales$get_scales("fill")$map(c("3:C", "2:B", "1:A")))

  # Neither works as a position; plot their values or labels instead.
  p <- ggplot2::ggplot(df, ggplot2::aes(v, y)) + ggplot2::geom_point()
  expect_error(ggplot2::ggplot_build(p))
  p <- ggplot2::ggplot(df, ggplot2::aes(d, y)) + ggplot2::geom_point()
  expect_error(ggplot2::ggplot_build(p))
  p <- ggplot2::ggplot(df, ggplot2::aes(node_values(v), y)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleContinuousPosition")
  expect_equal(b$data[[1]]$x, c(20.5, 10, 30))
  p <- ggplot2::ggplot(df, ggplot2::aes(format(d), y)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), c("1:A", "2:B", "3:C"))
})

test_that("a data-frame-backed node_vec is a single data.frame column", {
  n <- node_vec(data.frame(id = 1:3, lab = c("A", "B", "C")), 1:2, 2:3)
  df <- data.frame(v = n, y = 1:3)
  expect_equal(names(df), c("v", "y"))
  expect_equal(nrow(df), 3L)
  expect_s3_class(df[2:3, ]$v, "node_vec")
  expect_equal(format(df[2:3, ]$v), c("2:B", "3:C"))
  expect_equal(edge_pairs(df[2:3, ]$v), "1->2")
  expect_output(print(df), "1:A")
  expect_equal(names(as.data.frame(n)), "n")
})

# -- graph identity (_dev/graph-identity.md) -------------------------------

test_that("a node_vec slice keeps its graph and only sees the induced edges", {
  n <- node_vec(c("A", "B", "C", "A"), from = c(1L, 2L, 3L), to = c(2L, 3L, 1L))
  s <- n[c(3, 1, 2)]
  expect_identical(attr(s, "graph"), attr(n, "graph"))
  expect_equal(format(edges(s)), c("[A]->[B]", "[B]->[C]", "[C]->[A]"))
  expect_equal(edge_pairs(s), c("2->3", "3->1", "1->2"))
  expect_equal(edge_pairs(n[c(1, 2, 4)]), "1->2")
  expect_equal(n_edges(n[c(1, 2, 4)]), 1L)
  expect_equal(node_degree(n[c(1, 2, 4)]), c(1L, 1L, 0L))
  expect_equal(n_nodes(n[2:3]), 2L)
  # A slice covering the whole graph in order reads the graph as is.
  expect_identical(backend_of(n[1:4]), attr(n, "graph"))
})

test_that("c() puts slices of the same graph back together", {
  n <- node_vec(c("A", "B", "C", "A"), from = c(1L, 2L, 3L), to = c(2L, 3L, 1L))
  # The B->C edge crossing between the two halves comes back.
  expect_equal(c(n[1:2], n[3:4]), n)
  expect_identical(attr(c(n[1:2], n[3:4]), "graph"), attr(n, "graph"))
  expect_equal(c(n[3:4], n[1:2]), n[c(3, 4, 1, 2)])
  expect_equal(do.call(c, as.list(n)), n)
  # The same nodes from two inputs are a disjoint union.
  u <- c(n, n)
  expect_equal(length(u), 8L)
  expect_equal(edge_pairs(u), c("1->2", "2->3", "3->1", "5->6", "6->7", "7->5"))
  # Overlapping slices are separate copies, each with its own edges.
  expect_equal(edge_pairs(c(n[1:2], n[2:3])), c("1->2", "3->4"))
  # A repeat within one slice is already a copy, a graph of its own, so it
  # doesn't go back into `n`'s graph.
  expect_equal(n_edges(c(n[c(1, 1)], n[2])), 0L)
  expect_equal(edge_pairs(n[c(1, 1, 2)]), "1->3")
})

test_that("[<- with nodes of the same graph keeps every edge", {
  n <- node_vec(c("A", "B", "C", "A"), from = c(1L, 2L, 3L), to = c(2L, 3L, 1L))
  x <- n
  x[2] <- x[2]
  expect_equal(x, n)
  x <- n
  x[[3]] <- n[[3]]
  expect_equal(x, n)
  # Swapping two nodes moves their edges with them.
  x <- n
  x[1:2] <- n[2:1]
  expect_equal(format(x), c("B", "A", "C", "A"))
  expect_equal(format(edges(x)), format(edges(n)))
  # A node the vector still holds elsewhere is a disjoint copy.
  x <- n
  x[2] <- n[3]
  expect_equal(format(x), c("A", "C", "C", "A"))
  expect_equal(format(edges(x)), "[C]->[A]")
  # Plain values relabel nodes and keep the graph.
  x <- n
  x[2] <- "Z"
  expect_identical(attr(x, "graph"), attr(n, "graph"))
  expect_equal(format(edges(x)), c("[A]->[Z]", "[Z]->[C]", "[C]->[A]"))
})

test_that("node_vecs of different graphs combine as a disjoint union in row order", {
  n <- node_vec(c("A", "B", "C"), from = 1:2, to = 2:3)
  m <- node_vec(c("X", "Y"), from = 2L, to = 1L)
  x <- c(n[1:2], m, n[3])
  expect_equal(format(x), c("A", "B", "X", "Y", "C"))
  expect_equal(format(edges(x)), c("[A]->[B]", "[B]->[C]", "[Y]->[X]"))
})

test_that("node_vec errors on edge endpoints that aren't nodes", {
  for (bad in c(0L, -1L, 3L)) {
    expect_error(node_vec(c("A", "B"), from = bad, to = 1L), "between 1 and 2")
    expect_error(node_vec(c("A", "B"), from = 1L, to = bad), "between 1 and 2")
    expect_error(
      new_node_vec(c("A", "B"), edges = data.frame(from = bad, to = 1L)),
      "between 1 and 2"
    )
  }
  expect_error(node_vec(c("A", "B"), from = NA_integer_, to = NA_integer_), "missing")
  expect_error(node_vec(c("A", "B"), from = 1L, to = NA_integer_), "missing")
  expect_error(node_vec(character(), from = 1L, to = 1L), "between 1 and 0")
})

test_that("node queries error on positions outside the graph", {
  d <- node_vec(c("a", "b"), c(1L, 1L), c(1L, 2L))
  for (bad in list(0, 5, -1, NA)) {
    expect_error(node_neighbors(d, bad), "between 1 and 2")
    expect_error(edge_incident(d, bad), "between 1 and 2")
    expect_error(node_incident(d, bad), "between 1 and 2")
  }
  expect_error(node_neighbors(d, c(1, 5)), "between 1 and 2")
  expect_equal(node_neighbors(d, 1:2), list(1:2, integer()))
  e <- edges(d)
  expect_error(node_neighbors(e, 3), "between 1 and 2")
  expect_error(node_incident(e, 3), "edge positions between 1 and 2")
  expect_error(attr(d, "graph")$neighbors_many(0L, "out"), "between 1 and 2")
  expect_error(attr(d, "graph")$neighbors_many(c(1L, 3L), "out"), "between 1 and 2")
  expect_error(attr(d, "graph")$edge_endpoints(c(1L, 3L)), "between 1 and 2")
  expect_error(attr(d, "graph")$edge_endpoints(0L), "between 1 and 2")
  expect_error(attr(d, "graph")$has_edge(1L, 3L), "between 1 and 2")
})

test_that("node_vec() errors on an edge attribute named like a misspelt option", {
  expect_snapshot(error = TRUE, node_vec(c("A", "B"), 1L, 2L, directd = FALSE))
  # `nodes` isn't an option of node_vec(), so it's an ordinary attribute.
  g <- node_vec(c("A", "B"), 1L, 2L, nodes = 1, weight = 2, directed = FALSE)
  expect_equal(edges(g)$nodes, 1)
  expect_equal(edges(g)$weight, 2)
  expect_false(attr(g, "directed"))
})

test_that("node_values() gives a node_vec's plain values", {
  n <- node_vec(factor(c("A", "B", "A")), 1:2, 2:3)
  expect_identical(node_values(n), factor(c("A", "B", "A")))
  expect_identical(node_values(n[3:1]), factor(c("A", "B", "A")))
  expect_identical(node_values(c(n, n)), factor(rep(c("A", "B", "A"), 2)))
  expect_equal(node_values(n) %in% "A", c(TRUE, FALSE, TRUE))

  d <- data.frame(id = 1:2, lab = c("a", "b"))
  expect_identical(node_values(node_vec(d, 1L, 2L)), d)
  h <- node_vec(c("A", "B", "C"), from = list(1:2), to = list(3L))
  expect_identical(node_values(h), c("A", "B", "C"))

  # Plain values are already values.
  expect_identical(node_values(c("A", "B")), c("A", "B"))
  expect_identical(node_values(d), d)

  e <- edges(n)
  expect_error(node_values(e), "nodes\\(x\\)")
  expect_error(node_values(agg_vec(c("A", NA))), "format")
})

test_that("a factor node_vec keeps its own levels, in plots too", {
  n <- node_vec(factor(c("B", "A"), levels = c("C", "B", "A")), 1L, 2L)
  expect_equal(levels(n), c("C", "B", "A"))
  d <- droplevels(n)
  expect_equal(levels(d), c("B", "A"))
  expect_equal(d == n, c(TRUE, TRUE))
  expect_equal(edge_pairs(d), edge_pairs(n))
  expect_equal(levels(node_vec(c("B", "A"))), c("A", "B"))

  skip_if_not_installed("ggplot2")
  df <- data.frame(y = 1:2)
  df$n <- n
  p <- ggplot2::ggplot(df, ggplot2::aes(n, y, colour = n)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), c("B", "A"))
  expect_equal(b$plot$scales$get_scales("colour")$get_labels(), c("B", "A"))
  p <- p + ggplot2::scale_x_discrete(drop = FALSE)
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), c("C", "B", "A"))
})

test_that("node values don't carry the origin attribute of combined copies", {
  x <- node_vec(c("a", "b", "c"), c(1L, 2L), c(2L, 3L))
  expect_null(attributes(node_vec_data(vctrs::vec_c(x, x))))
  expect_null(attributes(node_vec_data(c(x, x))))
  expect_null(attributes(node_values(x[c(1, 1)])))
  d <- node_vec(data.frame(v = c("a", "b")), 1L, 2L)
  expect_named(attributes(node_vec_data(vctrs::vec_c(d, d))), c("names", "row.names", "class"), ignore.order = TRUE)
})

test_that("`[` and `[<-` select a named node_vec's nodes by name", {
  x <- node_vec(c(a = "A", b = "B", c = "C"), 1L, 2L)
  expect_equal(node_values(x["a"]), c(a = "A"))
  expect_equal(node_values(x[c("b", "a")]), c(b = "B", a = "A"))
  expect_equal(n_edges(x[c("b", "a")]), 1L)
  # As for a base vector, an unknown name selects a missing node.
  expect_equal(unname(node_values(x[c("a", "zz")])), c("A", NA))

  y <- x
  y["a"] <- x[3]
  expect_length(y, 3L)
  expect_equal(unname(node_values(y)), c("C", "B", "C"))
})

test_that("`[` on an edge_vec takes a character subscript as a missing edge", {
  e <- edge_vec(c(1L, 2L), c(2L, 1L), nodes = c("A", "B"))
  expect_true(is.na(e["x"]))
})

test_that("duplicated() on a node_vec keys repeated positions, missing nodes and missing values", {
  g <- node_vec(c("A", NA, "A", NaN), 1:2, 2:3)
  # Equal values at different positions aren't duplicates, nor are NA and
  # NaN values; repeats of one position are.
  expect_equal(duplicated(g), c(FALSE, FALSE, FALSE, FALSE))
  x <- g[c(2, 1, 2, 3, 1, 4)]
  expect_equal(duplicated(x), c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(duplicated(x, fromLast = TRUE), c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(anyDuplicated(x), 3L)
  expect_equal(anyDuplicated(x, fromLast = TRUE), 2L)
  # Missing nodes are duplicates of one another only.
  m <- vctrs::vec_c(g[1], vctrs::vec_init(g, 2), g[2])
  expect_equal(duplicated(m), c(FALSE, FALSE, TRUE, FALSE))
  # Nodes of separately built graphs never duplicate one another.
  h <- node_vec(c("A", NA, "A", NaN), 1:2, 2:3)
  expect_equal(duplicated(c(g, h)), rep(FALSE, 8))
})

