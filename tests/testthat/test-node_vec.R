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

test_that("`[.node_vec` clones a hyperedge once per combination of replicated members", {
  g <- node_vec(x = c("A", "B", "C"), from = list(c(1L, 2L)), to = 3L)
  m <- g[c(1, 1, 2, 3)] # A has 2 replicas (1, 2), B has 1 (3), C is now 4
  expect_equal(
    attr(m, "edges")$from,
    list(c(1L, 3L), c(2L, 3L)),
    ignore_attr = TRUE
  )
  expect_equal(attr(m, "edges")$to, c(4L, 4L))
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

test_that("`[.node_vec` clones incident edges when a node is replicated", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
  m <- g[c(1, 1, 2)]
  expect_length(m, 3L)
  expect_equal(format(m), c("A", "A", "B"))
  # Rewritten (see above): check via edges()/format(), not internal positions.
  expect_equal(format(edges(m)), c("[A]->[B]", "[A]->[B]"))
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
  # dynamically registered vec_proxy.node_vec().
  skip_if_not_installed("tibble")
  skip_if_not_installed("vctrs")
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

test_that("unique.node_vec() drops duplicate-valued nodes and their incident edges, via `[`", {
  g <- node_vec(x = c("A", "A", "B"), from = c(1L, 2L), to = c(2L, 3L))
  u <- unique(g)
  expect_equal(format(u), c("A", "B"))
  # The edge between the two "A" duplicates is dropped, not redirected onto
  # the surviving node, since duplicate 2 (the endpoint) no longer exists.
  # Rewritten (see above): attr(u, "edges")$to was always NULL for the
  # ordinary case, making the original assertion pass vacuously -- check via
  # edges() instead, which does exercise the drop.
  expect_length(edges(u), 0L)
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

test_that("rep.node_vec() clones a replicated node's incident edges, like `[` does", {
  g <- node_vec(x = c("A", "B"), from = 1L, to = 2L, weight = 42)
  r <- rep(g, 2)
  expect_equal(format(r), c("A", "B", "A", "B"))
  # rep(x, 2) tiles the whole vector (positions 1,2,1,2), so the A->B edge
  # is cloned once per combination of A's replicas (1, 3) and B's (2, 4).
  # Rewritten (see above): check via edges()/format(), not internal positions.
  expect_equal(format(edges(r)), rep("[A]->[B]", 4))
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
  skip_if_not_installed("vctrs")
  expect_equal(x, vctrs::vec_assign(n, 2:3, m))

  nd <- node_vec(data.frame(id = 1:3), from = 1:2, to = 2:3)
  df <- tibble::tibble(x = nd)
  df$x[2] <- nd[3]
  expect_equal(format(df$x), c("1", "3", "3"))
})

test_that("element access on a data-frame node_vec works on nodes, not columns", {
  nd <- node_vec(data.frame(id = c(1L, 1L, 2L), lab = c("A", "A", "B")), 1:2, 2:3)
  expect_equal(nd[[3]], data.frame(id = 2L, lab = "B"))
  expect_error(nd[[4]])
  expect_length(as.list(nd), 3L)
  expect_equal(as.list(nd)[[2]], data.frame(id = 1L, lab = "A"))
  expect_equal(vapply(nd, function(v) v$lab, character(1)), c("A", "A", "B"))
  expect_equal(duplicated(nd), c(FALSE, TRUE, FALSE))
  expect_equal(anyDuplicated(nd), 2L)
  expect_equal(format(unique(nd)), c("1:A", "2:B"))

  n1 <- node_vec(data.frame(id = c(5L, 6L, 5L)), 1L, 2L)
  expect_equal(n1[[3]], data.frame(id = 5L))
  expect_equal(duplicated(n1), c(FALSE, FALSE, TRUE))
})

test_that("element access on an atomic node_vec returns bare values", {
  n <- node_vec(c(a = "A", b = "B", c = "A"), 1:2, 2:3)
  expect_identical(n[[2]], "B")
  expect_identical(as.list(n), list(a = "A", b = "B", c = "A"))
  expect_equal(duplicated(n), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(n), 3L)
})

test_that("purrr::map() works element-wise on node_vecs", {
  skip_if_not_installed("purrr")
  nd <- node_vec(data.frame(id = 1:2, lab = c("A", "B")), 1L, 2L)
  expect_equal(purrr::map_chr(nd, ~ .x$lab), c("A", "B"))
  expect_equal(purrr::map_chr(node_vec(c("A", "B")), identity), c("A", "B"))
})

test_that("atomic node_vecs plot with the scale of their values", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(y = 1:3)
  df$v <- node_vec(c("A", "B", "C"), 1:2, 2:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(v, y)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleDiscretePosition")
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), c("A", "B", "C"))

  df$v <- node_vec(c(10, 20.5, 30), 1:2, 2:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(v, y)) + ggplot2::geom_point()
  b <- expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleContinuousPosition")
  expect_equal(b$data[[1]]$x, c(10, 20.5, 30))
})

test_that("data-frame node_vecs can't be plotted without format()", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(y = 1:3)
  df$n <- node_vec(data.frame(lab = c("A", "B", "C")), 1:2, 2:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(n, y)) + ggplot2::geom_point()
  expect_error(ggplot2::ggplot_build(p), "use format\\(\\)")
  p <- ggplot2::ggplot(df, ggplot2::aes(format(n), y)) + ggplot2::geom_point()
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), c("A", "B", "C"))
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
