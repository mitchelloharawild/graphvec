# Subset a node_vec

Slicing a `node_vec` is an induced subgraph: edges that lose an endpoint
are dropped, surviving edges are remapped to the new positions, and
replicated nodes (e.g. `x[c(1, 1, 2)]`) clone the edges incident to the
original.

## Usage

``` r
# S3 method for class 'node_vec'
x[i, ...]
```

## Arguments

- x:

  A `node_vec`.

- i:

  Indices to select, as for `` `[` ``.

- ...:

  Passed on.

## Value

A `node_vec` containing only the selected nodes, with `edges` restricted
to the induced subgraph.

## Examples

``` r
g <- node_vec(
  x = c("A", "B", "C"),
  from = c(1L, 2L),
  to = c(2L, 3L)
)
g[1:2]
#> <node_vec[2]>
#> [1] A B
```
