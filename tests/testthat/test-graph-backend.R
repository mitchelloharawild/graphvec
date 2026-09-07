# Sanity checks on the trickier GraphBackend semantics called out in
# _dev/RUST_BACKEND.md (self-loop degree counting, edge-id order stability).
# These are also covered by Rust-level tests in src/rust/src/lib.rs, but
# `cargo test` doesn't run as part of `devtools::test()`/R CMD check, so
# these R-level equivalents are what actually exercises them in CI.

test_that("edge_endpoints() preserves construction order", {
  from <- c(3L, 1L, 2L, 1L)
  to <- c(1L, 2L, 3L, 3L)
  g <- GraphBackend$new(3L, from, to, TRUE)
  ends <- g$edge_endpoints()
  expect_equal(ends$from, from)
  expect_equal(ends$to, to)
})

test_that("a self-loop counts twice towards undirected degree", {
  # Node 1 has a self-loop and one ordinary edge to node 2.
  g <- GraphBackend$new(2L, c(1L, 1L), c(1L, 2L), FALSE)
  expect_equal(g$degree(1L, "all"), 3L) # loop (2) + edge to 2 (1)
  expect_equal(g$degree(2L, "all"), 1L)
  expect_equal(sort(g$neighbors(1L, "all")), c(1L, 1L, 2L))
})

test_that("a directed self-loop counts once per direction, not doubled", {
  g <- GraphBackend$new(1L, 1L, 1L, TRUE)
  expect_equal(g$degree(1L, "out"), 1L)
  expect_equal(g$degree(1L, "in"), 1L)
  expect_equal(g$degree(1L, "all"), 2L)
})

test_that("has_edge() checks both orientations only when undirected", {
  gu <- GraphBackend$new(2L, 1L, 2L, FALSE)
  expect_true(gu$has_edge(1L, 2L))
  expect_true(gu$has_edge(2L, 1L))

  gd <- GraphBackend$new(2L, 1L, 2L, TRUE)
  expect_true(gd$has_edge(1L, 2L))
  expect_false(gd$has_edge(2L, 1L))
})

test_that("induced_subgraph() drops dangling edges and clones replicated ones", {
  # Triangle 1->2->3->1; new nodes <- old 1, 1, 2 (node 1 replicated, node 3
  # dropped): edge 1->2 clones once per replica of 1, edges touching 3 vanish.
  g <- GraphBackend$new(3L, c(1L, 2L, 3L), c(2L, 3L, 1L), TRUE)
  remap <- g$induced_subgraph(c(1L, 1L, 2L))
  expect_equal(remap$from, c(1L, 2L))
  expect_equal(remap$to, c(3L, 3L))
  expect_equal(remap$source_edge, c(1L, 1L))
})

test_that("induced_subgraph() treats 0 as \"no source\"", {
  g <- GraphBackend$new(2L, 1L, 2L, TRUE)
  remap <- g$induced_subgraph(c(0L, 2L))
  expect_length(remap$from, 0L)
})
