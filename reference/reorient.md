# Reorient a graph vector

`nodes()` and `edges()` return the same underlying graph, enumerated
along the node or edge axis respectively, regardless of which
orientation `x` started in. Both directions are lossless and involutive:
node attributes, isolated nodes, and `directed` all survive, because
both orientations carry the same logical graph — reorientation only
changes which table the result is indexed by.

## Usage

``` r
# S3 method for class 'agg_df'
nodes(x, ...)

# S3 method for class 'agg_df'
edges(x, ...)

# S3 method for class 'agg_vec'
nodes(x, ...)

# S3 method for class 'agg_vec'
edges(x, ...)

# S3 method for class 'edge_vec'
edges(x, ...)

# S3 method for class 'edge_vec'
nodes(x, ...)

# S3 method for class 'node_vec'
nodes(x, ...)

# S3 method for class 'node_vec'
edges(x, ...)

nodes(x, ...)

edges(x, ...)
```

## Arguments

- x:

  A `node_vec` or `edge_vec`.

- ...:

  Passed on to methods.

## Value

`nodes()` returns a `node_vec`. `edges()` returns an `edge_vec`.
