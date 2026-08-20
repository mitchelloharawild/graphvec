# Changelog

## graphvec (development version)

Initial CRAN submission.

### New features

- Added
  [`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md),
  a graph vector of nodes with edges stored as attributes. Slicing
  induces a subgraph: edges that lose an endpoint are dropped, and
  remaining endpoints are remapped.
- Added
  [`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md),
  a graph vector of edges with node data stored as attributes. Slicing
  selects edges directly, leaving nodes unaffected.
- Added
  [`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md),
  an aggregation vector: a `node_vec` consisting of a parent value (the
  aggregated value) and its disaggregated children.
- Added
  [`agg_df()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_df.md),
  a table of
  [`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
  columns, one row per level of aggregation, which can be reoriented
  into a graph.
- Added
  [`nodes()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)/[`edges()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)
  generics to losslessly reorient a graph vector between node- and
  edge-indexed forms.
- Added
  [`is_aggregated()`](https://pkg.mitchelloharawild.com/graphvec/reference/is_aggregated.md)
  to test whether an element is an aggregation of smaller data.
- Added
  [`as.igraph()`](https://pkg.mitchelloharawild.com/graphvec/reference/as.igraph.md)
  methods for `node_vec`, `edge_vec`, `agg_vec`, and `agg_df`,
  converting them to
  [`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html)
  objects.
