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
