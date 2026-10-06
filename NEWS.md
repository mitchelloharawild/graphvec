# graphvec (development version)

## New features

* `agg_vec`, `node_vec` and `edge_vec` now work with vctrs, so they can be
  used as tibble columns, as tsibble keys, and in dplyr verbs such as
  `filter()`, `arrange()`, `group_by()` and `bind_rows()`. The vctrs
  methods are registered when vctrs is loaded, so vctrs is not a hard
  dependency.
* `vec_c()` and `bind_rows()` combine `node_vec`s and `edge_vec`s as a
  disjoint union of their graphs, the same as `c()`.
* `agg_vec` combines with character (and other base vectors) in either
  order with `vec_c()`. An all-`<aggregated>` `agg_vec` takes on the
  other side's value type.
* `agg_vec` sorts `<aggregated>` after every disaggregated value, with
  `vec_order()`, `dplyr::arrange()`, `order()` and `sort()`.
* Added `as.character()`, `unique()`, `duplicated()` and `rep()` methods for
  `agg_vec`, and a `rep()` method for `edge_vec`.
* Adding an `agg_vec` to a ggplot2 plot now gives an error suggesting
  `format()`, rather than defaulting to a continuous scale. ggplot2 is not
  a hard dependency.
* An atomic `node_vec` in a ggplot2 plot now uses the scale of its values
  (e.g. discrete for character nodes). Adding a data-frame `node_vec` or an
  `edge_vec` gives an error suggesting `format()`.
* Added `[[`, `[<-`, `[[<-` and `as.list()` methods for `agg_vec` and
  `edge_vec`, so assignment (including `df$col[i] <- value`) and
  `purrr::map()`/`lapply()` work element-wise. Assigning an `agg_vec`
  element can set `<aggregated>`.
* Added a `[<-` method for `node_vec`. A plain value relabels the selected
  nodes and keeps their edges; a `node_vec` value replaces them, with its
  own edges, as a disjoint union like `c()`. Assigning an `edge_vec` into
  an `edge_vec` is also a disjoint union.
* `edge_vec` equality now compares the node values at each end and the
  edge attributes, not node positions, so edge_vecs work as join keys and
  with `distinct()`/`count()` after `bind_rows()`. They sort by node values
  with `vec_order()` and `arrange()`, including hyperedges. Added
  `unique()` and `duplicated()` methods for `edge_vec` with the same
  semantics.
* testthat's `expect_equal()` (via `waldo::compare()`) now compares
  `node_vec`s and `edge_vec`s by their nodes, edges and `directed`, so
  equivalent graph vectors compare equal. waldo is not a hard dependency.

## Bug fixes

* `bind_rows()` no longer drops all edges of a `node_vec` column.
* A data-frame-backed `node_vec` can be a tibble column again.
* `format()` and `print()` on an `edge_vec` with no node data now label
  each endpoint by its node position (e.g. `[1]->[2]`), rather than
  returning nothing.
* An `edge_vec` with no node data now has a node for every position its
  edges reference, so `c()`, `vec_c()` and `bind_rows()` combine them as a
  disjoint union instead of overlapping their positions, `as.igraph()`
  counts every node, and `nodes()` labels nodes by position.
* `[[`, `as.list()`, `duplicated()` and `anyDuplicated()` on a
  data-frame-backed `node_vec` now work on its nodes (1-row data frames)
  rather than its columns, so `purrr::map()` and `lapply()` work
  element-wise.
* vctrs no longer treats `<aggregated>` as incomplete, so
  `tidyr::drop_na()` keeps `<aggregated>` rows, and `vec_equal()` gives
  `FALSE` rather than `NA` when comparing `<aggregated>` with a value.
* A data-frame-backed `node_vec` no longer reports its columns as
  element names, which made `vec_c()`, `bind_rows()`, joins and other vctrs
  functions fail with an internal vctrs error.
* Tab completion after `$` on an `edge_vec` offers `from` and `to` again.

## Breaking changes

* `agg_vec()` now errors when `aggregated` doesn't have the same length as
  `x`, rather than recycling it.

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
