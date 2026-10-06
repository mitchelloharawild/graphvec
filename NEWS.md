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

## Bug fixes

* `bind_rows()` no longer drops all edges of a `node_vec` column.
* A data-frame-backed `node_vec` can be a tibble column again.
* A data-frame-backed `node_vec` no longer reports its columns as
  element names, which made `vec_c()`, `bind_rows()`, joins and other vctrs
  functions fail with an internal vctrs error.

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
