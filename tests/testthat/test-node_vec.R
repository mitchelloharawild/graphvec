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

test_that("node_vec() stores edges as an attribute", {
  g <- node_vec(x = c("A", "B", "C"), from = c(1L, 2L), to = c(2L, 3L))
  stored <- attr(g, "edges")
  expect_equal(stored[["to"]], c(2L, 3L))
})

test_that("node_vec() accepts an empty vector", {
  g <- node_vec()
  expect_s3_class(g, "node_vec")
  expect_length(g, 0L)
})

test_that("node_vec() requires `from`/`to` to be plain integer vectors (hyperedges not yet supported)", {
  expect_error(node_vec(x = c("A", "B", "C"), from = list(1L), to = 2L))
  expect_error(node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L))
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
  expect_equal(attr(m, "edges")$to, 2L)
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
  expect_equal(attr(m, "edges")$weight, 2)
})

test_that("`[.node_vec` clones edge attributes when a node is replicated", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L, weight = 42)
  m <- g[c(1, 1, 2)]
  expect_equal(attr(m, "edges")$weight, c(42, 42))
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
  expect_equal(attr(m, "edges")$from, 1L)
  expect_equal(attr(m, "edges")$to, 2L)
})

test_that("`[.node_vec` drops edges that lose an endpoint", {
  g <- node_vec(
    x = c("A", "B", "C", "D"),
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L)
  )
  m <- g[c(1, 4)]
  expect_length(m, 2L)
  expect_length(attr(m, "edges")$to, 0L)
})

test_that("`[.node_vec` clones incident edges when a node is replicated", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  m <- g[c(1, 1, 2)]
  expect_length(m, 3L)
  expect_equal(format(m), c("A", "A", "B"))
  expect_equal(attr(m, "edges")$from, c(1L, 2L))
  expect_equal(attr(m, "edges")$to, c(3L, 3L))
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
  expect_equal(attr(filtered$g, "edges")$to, 2L)
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

test_that("a data-frame-backed node_vec can't be embedded as a tibble column (known limitation)", {
  # Embedding used to work via vec_proxy.node_vec()/vec_restore.node_vec():
  # without vctrs, tibble::tibble() requires each column to satisfy
  # vctrs::obj_is_vector(), which only recognises a classed *list* (as
  # opposed to a classed atomic vector, e.g. character- or factor-backed --
  # see the dplyr-column test above, which still works) as vector-like if it
  # implements vec_proxy(). A data-frame-backed node_vec is list-typed and no
  # longer registers one, so it's rejected outright rather than mis-rendered.
  skip_if_not_installed("tibble")
  g <- node_vec(
    x = data.frame(name = c("A", "B", "C"), size = c(10, 4, 7)),
    from = 1L, to = 2L
  )
  expect_error(tibble::tibble(id = 1:3, g = g))
})

test_that("sort(), rev(), head() route through `[` and inherit its induced-subgraph remap", {
  g <- node_vec(x = c("C", "A", "B"), from = c(1L, 2L), to = c(2L, 3L))

  s <- sort(g)
  expect_equal(format(s), c("A", "B", "C"))
  expect_equal(attr(s, "edges")$from, c(3L, 1L))
  expect_equal(attr(s, "edges")$to, c(1L, 2L))

  r <- rev(g)
  expect_equal(format(r), c("B", "A", "C"))

  h <- head(g, 2)
  expect_equal(format(h), c("C", "A"))
  expect_equal(attr(h, "edges")$to, 2L)
})

test_that("unique.node_vec() drops duplicate-valued nodes and their incident edges, via `[`", {
  g <- node_vec(x = c("A", "A", "B"), from = c(1L, 2L), to = c(2L, 3L))
  u <- unique(g)
  expect_equal(format(u), c("A", "B"))
  # The edge between the two "A" duplicates is dropped, not redirected onto
  # the surviving node, since duplicate 2 (the endpoint) no longer exists.
  expect_length(attr(u, "edges")$to, 0L)
})

test_that("c.node_vec() is a disjoint union: values concatenate, second graph's edges are offset", {
  g1 <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  g2 <- node_vec(x = c("X", "Y"), from = 1L, to = 2L)
  u <- c(g1, g2)

  expect_equal(format(u), c("A", "B", "X", "Y"))
  expect_equal(attr(u, "edges")$from, c(1L, 3L))
  expect_equal(attr(u, "edges")$to, c(2L, 4L))
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

test_that("rep.node_vec() clones a replicated node's incident edges, like `[` does", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L, weight = 42)
  r <- rep(g, 2)
  expect_equal(format(r), c("A", "B", "A", "B"))
  # rep(x, 2) tiles the whole vector (positions 1,2,1,2), so the A->B edge
  # is cloned once per combination of A's replicas (1, 3) and B's (2, 4).
  expect_equal(attr(r, "edges")$from, c(1L, 3L, 1L, 3L))
  expect_equal(attr(r, "edges")$to, c(2L, 2L, 4L, 4L))
  expect_equal(attr(r, "edges")$weight, rep(42, 4))
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

test_that("order() sorts a node_vec by node value, not position", {
  g <- node_vec(x = c(3, 1, 2), from = 1L, to = 2L)
  expect_equal(order(g), c(2L, 3L, 1L))
})

test_that("print.node_vec() shows a header and the formatted values, not raw attributes", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  expect_output(print(g), "<node_vec[2]>", fixed = TRUE)
  expect_output(print(g), "[1] A B", fixed = TRUE)
  expect_false(grepl("attr\\(,", paste(capture.output(print(g)), collapse = "\n")))
})
