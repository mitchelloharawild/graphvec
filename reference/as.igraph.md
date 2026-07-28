# Convert a graph vector to an igraph object

Methods for converting `node_vec`, `agg_vec`, and `edge_vec` objects to
[`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html)
objects using
[`igraph::graph_from_edgelist()`](https://r.igraph.org/reference/graph_from_edgelist.html).

## Usage

``` r
as.igraph.agg_vec(x, ...)

as.igraph.node_vec(x, ...)

as.igraph.edge_vec(x, ...)
```

## Arguments

- x:

  A `node_vec`, `agg_vec`, or `edge_vec` object.

- ...:

  Additional arguments (currently unused).

## Value

An
[`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html)
directed graph.

## See also

[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md),
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md),
[`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md)
