# Convert a graph vector to an igraph object

Methods for converting `node_vec`, `agg_vec`, `agg_df`, and `edge_vec`
objects to
[`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html)
objects using
[`igraph::graph_from_edgelist()`](https://r.igraph.org/reference/graph_from_edgelist.html).

## Usage

``` r
# S3 method for class 'agg_vec'
as.igraph(x, ...)

# S3 method for class 'agg_df'
as.igraph(x, ...)

# S3 method for class 'node_vec'
as.igraph(x, ...)

# S3 method for class 'edge_vec'
as.igraph(x, ...)
```

## Arguments

- x:

  A `node_vec`, `agg_vec`, `agg_df`, or `edge_vec` object.

- ...:

  Additional arguments (currently unused).

## Value

An
[`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html)
object.

## See also

[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md),
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md),
[`agg_df()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_df.md),
[`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md)

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- node_vec(
    x = c("A", "B", "C"),
    from = c(1L, 2L),
    to = c(2L, 3L)
  )
  igraph::as.igraph(g)
}
#> IGRAPH 78a56fd D--- 3 2 -- 
#> + edges from 78a56fd:
#> [1] 1->2 2->3
```
