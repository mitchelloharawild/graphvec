# graphvec (development version)

## Breaking changes

* A `node_vec` slice now keeps the graph it came from, plus the positions
  of its nodes in it, as an `edge_vec` slice already did. On its own a
  slice behaves as before (its edges are the induced subgraph on its
  nodes), but combining nodes of the same graph puts them back into that
  graph with every edge between them. Nodes only become a separate,
  disjoint copy where the same node comes from two different inputs, so
  `c(n, n)` and `vec_c(n, n)` are still a disjoint union, while
  `c(n[1:2], n[3:4])` now gives back `n`, including the edges between the
  two halves. This applies to `c()`, `[<-`, `[[<-` and every vctrs
  operation: `if_else(cond, n, n)`, `case_when()`, `coalesce()`,
  `replace()`, no-op `rows_patch()`/`rows_update()`, `x[2] <- x[2]` and
  `purrr::map_vec(n, identity)` now keep all of `n`'s edges instead of
  dropping those between rows from different inputs.
* Repeating nodes of a `node_vec` now makes disjoint copies of the graph,
  as `c()` does, rather than cloning every edge once per combination of
  its ends' repeats. `n[c(1:3, 1:3)]`, `vec_slice()`, `rep(n, 2)` and
  `vec_rep(n, 2)` are all `c(n, n)`, with one copy of each edge per
  repeat. The k-th occurrence of each node belongs to the k-th copy, so in
  `n[c(1, 1, 2)]` only the first `n[1]` keeps its edge to `n[2]`, and
  `rep(n, each = 2)`, `vec_rep_each()`, `slice(df, c(1, 1))`, joins that
  repeat rows and `tidyr::uncount()` give the same copies. Copies still
  equal the nodes they copy. A slice with repeats is a graph of its own,
  so `c(n[c(1, 1)], n[2])` no longer connects to `n[2]`.
* `c()`, `vec_c()`, `[<-` and `bind_rows()` of `edge_vec`s of the same
  graph now share it: the edges keep pointing at the same nodes, with one
  node table, instead of copying the nodes for every input. Edge_vecs of
  different graphs are still combined as a disjoint union, in row order.
  No-op `rows_patch()`, `full_join(by = e)`, `drop_na()` and `fill()` no
  longer grow the node table or disconnect the edges.
* `edge_vec` equality is now by graph and node positions (and edge
  attributes), not node values, with or without node data. Two edges are
  equal when they are edges of the same graph between the same nodes, so
  `vec_in()`, `duplicated()`, `unique()`, joins, `distinct()` and
  `count()` match edges of one graph (and edge_vecs without node data now
  match at all), but two separately built edge_vecs never match, even
  with identical labels: match on `format(e)` for that. Sorting is still
  by node values, with positions breaking ties. Hyperedges, which have no
  graph identity, still compare by value.
* `==` and `!=` between two `node_vec`s now compare nodes as `vec_equal()`
  and `duplicated()` do, by graph, position and value, rather than by
  value alone: `n == n[3:1]` is no longer `TRUE` for different nodes with
  the same label. Comparing a `node_vec` with a plain value (`n == "A"`)
  is now an error, as are `<`, arithmetic and every other operator, as for
  an `edge_vec`: use `node_values(n) == "A"` to compare node values.
* On R >= 4.3, `match()` and `%in%` on `node_vec`s now agree with
  `vec_match()` and `vec_in()`, matching nodes by graph, position and value
  rather than by value alone. A node never matches a plain value, so
  `match(n, "A")` and `n %in% "A"` find nothing: compare
  `node_values(n)` for that. Hyperedge `node_vec`s still match by value.

## New features

* New `node_values()` gives a `node_vec`'s plain values (an atomic vector
  or a data frame), without its graph or the nodes' identity, for comparing
  nodes by value explicitly: `node_values(n) %in% c("A", "B")`.
* `agg_vec`, `node_vec` and `edge_vec` now work with vctrs, so they can be
  used as tibble columns, as tsibble keys, and in dplyr verbs such as
  `filter()`, `arrange()`, `group_by()` and `bind_rows()`. vctrs is now
  imported, so these methods are always registered.
* `vec_c()` and `bind_rows()` combine `node_vec`s and `edge_vec`s the same
  way as `c()` (see Breaking changes).
* `agg_vec` combines with character (and other base vectors) in either
  order with `vec_c()`. An all-`<aggregated>` `agg_vec` takes on the
  other side's value type.
* `agg_vec` sorts `<aggregated>` after every disaggregated value, with
  `vec_order()`, `dplyr::arrange()`, `order()` and `sort()`.
* Added `as.character()`, `unique()`, `duplicated()` and `rep()` methods for
  `agg_vec`, and a `rep()` method for `edge_vec`.
* `agg_vec`, `node_vec` and `edge_vec` can be used in ggplot2 plots
  rather than erroring. Their `scale_type()` names their own type first
  (`"agg"`, `"node"`, `"edge"`), so an extension package defining e.g.
  `scale_x_node()` or `scale_colour_edge()` provides their default scales.
  Otherwise an `edge_vec` or `agg_vec` gets ggplot2's discrete scales,
  labelled by `format()` in sort order (with `<aggregated>` as the last
  level), which work for colour, fill, shape and the like but not for the
  x and y positions: plot `format(x)` there. A `node_vec` takes the scale
  type of its values, so character, factor and logical nodes get discrete
  scales on every aesthetic, and dates and date-times get date scales.
  Numeric (and data-frame) nodes have no arithmetic, so get discrete
  scales rather than continuous ones, and don't work on x and y: plot
  `node_values(n)` there. `levels()` on any graph vector gives the labels
  its discrete scales show, except that a factor `node_vec` keeps its own
  levels (and `droplevels()` drops its unused ones). ggplot2 is not a hard
  dependency.
* Added `[[`, `[<-`, `[[<-` and `as.list()` methods for `agg_vec` and
  `edge_vec`, so assignment (including `df$col[i] <- value`) and
  `purrr::map()`/`lapply()` work element-wise. Assigning an `agg_vec`
  element can set `<aggregated>`.
* Added a `[<-` method for `node_vec`. A plain value relabels the selected
  nodes and keeps their edges; a `node_vec` value replaces them, combined
  with the rest of `x` the same way as `c()`. Assigning an `edge_vec` into
  an `edge_vec` also combines them like `c()`.
* `edge_vec`s work as join keys and with `distinct()`/`count()`, by graph
  and position (see Breaking changes). They sort by node values with
  `vec_order()` and `arrange()`, including hyperedges. Added `unique()`
  and `duplicated()` methods for `edge_vec` with the same semantics.
* testthat's `expect_equal()` (via `waldo::compare()`) now compares
  `node_vec`s and `edge_vec`s by their nodes, edges and `directed`, so
  equivalent graph vectors compare equal. waldo is not a hard dependency.

## Bug fixes

* `node_vec()`, `edge_vec()` and their `new_*()` constructors now error on
  an edge endpoint that isn't a node position, rather than crashing R (0,
  negative or `NA`), dropping the edge from a `node_vec`, or growing an
  `edge_vec`'s graph past its node data. A position must be in
  `1:length(x)` for a `node_vec`, or `1:NROW(nodes)` for an `edge_vec` with
  node data; without node data an `edge_vec` still takes its node count
  from the largest position. `NA` at both ends is still a missing edge in
  an `edge_vec`, but an error in a `node_vec`.
* `node_vec()`, `edge_vec()` and `new_edge_vec()` now error on an edge
  attribute named like a misspelling of `nodes` or `directed` (e.g.
  `directd = FALSE` or `node = ...`), suggesting the intended option,
  rather than silently keeping it as an edge attribute.
* `node_neighbors()`, `node_parents()`, `node_children()`,
  `edge_incident()` and `node_incident()` now error on an `i` outside the
  graph's nodes (or edges, for `node_incident()`) instead of quietly
  returning an empty or `NA` result.
* A `node_vec` or `edge_vec` now works after `saveRDS()`/`readRDS()`,
  `serialize()`/`unserialize()` or in a callr/future worker, where its
  graph used to be lost: `node_degree()` and `edges()` errored and an
  `edge_vec` couldn't print. The graph is rebuilt on first use and keeps
  its identity, so vectors saved from the same graph still are the same
  graph after loading, and different graphs never compare equal.
* `nodes()` and `edges()` on an `agg_df` no longer add false parent edges
  when values run together across columns (e.g. `x:yz:q` as a child of
  `xy:z:<aggregated>`), and now tell `<aggregated>`, a genuine `NA` and the
  string `"NA"` apart, and compare doubles exactly rather than as printed
  (so `0.1 + 0.2` is not `0.3`). vctrs is now imported rather than
  suggested.
* `nodes()` and the topology functions (`node_degree()`,
  `node_neighbors()`, ...) on an `edge_vec` with a missing edge (e.g. from
  `vec_init()`, `lag()` or a join) no longer fail in the graph backend; the
  missing edge joins no nodes and is left out.
* `bind_rows()` no longer drops all edges of a `node_vec` column.
* A data-frame-backed `node_vec` can be a tibble column again.
* `format()` and `print()` on an `edge_vec` with no node data now label
  each endpoint by its node position (e.g. `[1]->[2]`), rather than
  returning nothing.
* An `edge_vec` with no node data now has a node for every position its
  edges reference, so `c()`, `vec_c()` and `bind_rows()` combine them as a
  disjoint union instead of overlapping their positions, `as.igraph()`
  counts every node, and `nodes()` labels nodes by position.
* `duplicated()` and `anyDuplicated()` on a data-frame-backed `node_vec`
  now work on its nodes (rows) rather than its columns.
* vctrs no longer treats `<aggregated>` as incomplete, so
  `tidyr::drop_na()` keeps `<aggregated>` rows, and `vec_equal()` gives
  `FALSE` rather than `NA` when comparing `<aggregated>` with a value.
* A data-frame-backed `node_vec` no longer reports its columns as
  element names, which made `vec_c()`, `bind_rows()`, joins and other vctrs
  functions fail with an internal vctrs error.
* Tab completion after `$` on an `edge_vec` offers `from` and `to` again.
* `==` and `!=` on an `edge_vec` now compare edges as `vec_equal()` does
  (by graph and node positions), rather than returning an empty result;
  other operators error. `anyDuplicated()` on an `edge_vec` or `agg_vec`
  now finds duplicates as `duplicated()` does, rather than returning 0.
  On R >= 4.3, `match()` and `%in%` on an `agg_vec`, or an `edge_vec` with
  edge attributes, now agree with `vec_match()` and `vec_in()`; for an
  `edge_vec` without edge attributes, use `vec_match()` and `vec_in()`.
* `==` on an `agg_vec` no longer treats a genuine `NA` as equal to
  `<aggregated>`: `<aggregated>` only equals `<aggregated>`, and missing
  values compare as `NA`, as for base vectors (`!=` likewise).
* `c()`, `vec_c()` and `bind_rows()` of a `node_vec` or `edge_vec` with
  edge attributes and one with no edges no longer error with "replacement
  has 1 row, data has 0".
* `agg_vec()` now errors on a missing value in `aggregated`, rather than
  silently dropping the value at that position.
* `as.igraph()` on an `agg_vec` now gives the same graph as `nodes()`,
  linking every disaggregated value to the first `<aggregated>` value,
  rather than an older row-order model where each value only joined the
  `<aggregated>` values just before it, a value with none before it had no
  parent, and consecutive `<aggregated>` values were a hyperedge error.
* `format()`, `as.character()` and `print()` on an `edge_vec` no longer
  pad node labels to a common width inside each edge's label, so an edge's
  label doesn't depend on the other edges: `format(e[i])` is
  `format(e)[i]`, as matching edges across graphs by `format(e)` needs.
  `format(e)` now gives `"[A]->[BBB]" "[C]->[A]"` rather than
  `"[A]->[BBB]" "[C]->[A  ]"`; printing still aligns the labels as a whole.
* `[` on a named `node_vec` now selects nodes by name (`x["a"]`), as `[[`
  already did, rather than returning a missing node, and `x["a"] <- value`
  with a `node_vec` `value` now replaces the named node rather than
  appending one. An unknown name still gives a missing node, as for base
  vectors.
* graphvec now declares the Rust version it really needs, rustc >= 1.85
  (the minimum of its Rust dependencies), rather than 1.65, so installing
  with an older toolchain fails up front with a clear message instead of
  partway through compiling.
* `edge_multiplicity()` now gives `NA` for a missing edge (e.g. from
  `vec_init()`) and leaves it out of the grouping, rather than counting
  missing edges as parallel to each other, and `edge_is_multi()` is `NA`
  for it too. `as.igraph()` on an `edge_vec` with a missing edge now
  errors clearly, rather than passing `NA`s to igraph. `n_edges()` still
  counts missing edges, as elements of the `edge_vec`, while
  `node_degree()` ignores them; both are now documented.
* `as.igraph()` now keeps node values and edge attributes, rather than
  dropping them. Following igraph's convention, a vector of node values
  becomes the `name` vertex attribute and each column of a data frame of
  node values a vertex attribute of its own, keeping its type; edge
  attribute columns become igraph edge attributes. An `agg_vec` or
  `agg_df` gives its columns (`value` for an `agg_vec`) as vertex
  attributes.

## Breaking changes

* `agg_vec()` now errors when `aggregated` doesn't have the same length as
  `x`, rather than recycling it.
* `[[` on a `node_vec` now returns a length-1 `node_vec` (the same as
  `x[i]`), like `[[` on `agg_vec` and `edge_vec`, rather than the node's
  plain value. `as.list()` gives a list of length-1 `node_vec`s, so
  `purrr::map()` and `lapply()` work element-wise, and `purrr::map_vec()`
  and `purrr::modify()` return a `node_vec`. A data-frame-backed `node_vec`
  works on its nodes, not its columns. Use `format()` to get node labels
  as plain strings. Added a matching `[[<-` method.
* `[[` on an `agg_vec`, `node_vec` or `edge_vec` now errors for an
  out-of-bounds index, as for base vectors, rather than returning a missing
  or empty element.

# graphvec 0.1.0

Initial CRAN submission.

## New features

* Added `node_vec()`, a graph vector of nodes with edges stored as
  attributes. Slicing induces a subgraph: edges that lose an endpoint are
  dropped, and remaining endpoints are remapped.
* Added `edge_vec()`, a graph vector of edges with node data stored as
  attributes. Slicing selects edges directly, leaving nodes unaffected.
* Added `agg_vec()`, an aggregation vector: a `node_vec` consisting of a
  parent value (the aggregated value) and its disaggregated children.
* Added `agg_df()`, a table of `agg_vec()` columns, one row per level of
  aggregation, which can be reoriented into a graph.
* Added `nodes()`/`edges()` generics to losslessly reorient a graph vector
  between node- and edge-indexed forms.
* Added `is_aggregated()` to test whether an element is an aggregation of
  smaller data.
* Added `as.igraph()` methods for `node_vec`, `edge_vec`, `agg_vec`, and
  `agg_df`, converting them to `igraph::igraph()` objects.
* Added a first set of graph operations, computed directly from the shared
  graph backend that a `node_vec`/`edge_vec` carries. Each accepts either
  orientation, and its return shape is fixed by its name rather than by an
  argument:
  * scalars: `n_nodes()`, `n_edges()`, `graph_density()`,
    `graph_is_directed()`, `graph_has_loops()`.
  * node-aligned: `node_degree()`, `node_is_isolated()`, `node_is_root()`,
    `node_is_leaf()`.
  * edge-aligned: `edge_heads()`, `edge_tails()`, `edge_is_loop()`,
    `edge_multiplicity()`, `edge_is_multi()`.
  * selections: `node_neighbors()`, `node_parents()`, `node_children()`,
    `edge_incident()`, `node_incident()`.
