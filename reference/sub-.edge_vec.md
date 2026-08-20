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
