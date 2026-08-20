# Subset an edge_vec

Slicing an `edge_vec` selects edges directly: dropping or reordering
edges never invalidates a node reference, so `nodes`/`directed` are
unaffected – unlike slicing a
[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md),
no remap is needed.

## Usage

``` r
# S3 method for class 'edge_vec'
x[i, ...]
```

## Arguments

- x:

  An `edge_vec`.

- i:

  Indices to select, as for `` `[` ``.

- ...:

  Passed on.

## Value

An `edge_vec` containing only the selected edges.

## Examples

``` r
g <- edge_vec(
  from = c(1L, 2L, 1L, 3L),
  to = c(2L, 3L, 3L, 1L),
  nodes = data.frame(label = c("A", "B", "C"))
)
g[1:2]
#> <edge_vec[2]>
#> [1] [A]->[B] [B]->[C]
```
