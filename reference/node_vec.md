# Graph vector along nodes

A `node_vec` is a vector of graph nodes with associated edges stored as
attributes.

## Usage

``` r
node_vec(x = list(), from = integer(), to = integer(), ..., directed = TRUE)
```

## Arguments

- x:

  A vector representing the nodes in the graph.

- from:

  Integer vector of 'from' node positions into `x`. Hyperedges (multiple
  'from' nodes per edge) are not yet supported.

- to:

  Integer vector of 'to' node positions into `x`.

- ...:

  Named edge attribute vectors (e.g. `weight = c(1, 2, 5)`), recycled to
  the number of edges. `from` and `to` are reserved and cannot be used
  as attribute names. Attribute columns are stored on the edge table
  itself, so they slice, replicate, and reorient with the edges they
  belong to (see
  [`nodes()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)/[`edges()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)).

- directed:

  A single logical value: is incidence ordered (`from` -\> `to`) or
  symmetric?

## Value

A `node_vec` object.

## Examples

``` r

g <- node_vec(
 x = factor(c("A", "B", "C")),
 from = c(1L, 2L, 1L),
 to = c(2L, 3L, 1L),
 weight = c(1, 2, 5)
)
g
#> <node_vec[3]>
#> [1] A B C

igraph::as.igraph(g)
#> IGRAPH 4bf4c72 D--- 3 3 -- 
#> + edges from 4bf4c72:
#> [1] 1->2 2->3 1->1
```
