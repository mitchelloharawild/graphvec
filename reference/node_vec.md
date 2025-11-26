# Graph vector along nodes

A `node_vec` is a vector of graph nodes with associated edges stored as
attributes.

## Usage

``` r
node_vec(x = list(), edges = data.frame(from = list(), to = integer()))
```

## Arguments

- x:

  A vector representing the nodes in the graph.

- edges:

  A data frame with columns `from` and `to` representing the edges

## Examples

``` r
g <- node_vec(
 x = factor(c("A", "B", "C")),
 edges = data.frame(
  from = list(c(1L), c(2L), c(1L, 2L)),
  to = c(2L, 3L, 1L)
 )
)
#> Error in data.frame(from = list(c(1L), c(2L), c(1L, 2L)), to = c(2L, 3L,     1L)): arguments imply differing number of rows: 2, 3

```
