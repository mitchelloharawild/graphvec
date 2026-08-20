# Graph vector along edges

An `edge_vec` is a vector of graph edges with associated node data
stored as attributes.

## Usage

``` r
edge_vec(
  from = integer(),
  to = integer(),
  ...,
  nodes = data.frame(),
  directed = TRUE
)
```

## Arguments

- from:

  Integer vector of 'from' node indices. Hyperedges (multiple 'from'
  nodes per edge) are not yet supported.

- to:

  Integer vector of 'to' node indices.

- ...:

  Named edge attribute vectors (e.g. `weight = c(1, 2, 5)`), recycled to
  the number of edges. `from` and `to` are reserved and cannot be used
  as attribute names. Attribute columns are stored on the edge table
  itself, so they slice, replicate, and reorient with the edges they
  belong to (see
  [`nodes()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)/[`edges()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)).

- nodes:

  Vector of node data (any vector, including a data frame of node
  attributes). Its size should be at least the maximum value in `from`
  and `to`.

- directed:

  A single logical value: is incidence ordered (`from` -\> `to`) or
  symmetric?

## Value

An `edge_vec` object.

## Examples

``` r
g <- edge_vec(
  from = c(1L, 2L, 1L, 3L),
  to = c(2L, 3L, 3L, 1L),
  weight = c(1, 2, 5, 3),
  nodes = data.frame(
    id = 1:3,
    label = c("A", "B", "C")
  )
)

# Access node data via `$`
g$from$label
#> [1] "A" "B" "A" "C"
g$to$label
#> [1] "B" "C" "C" "A"

# Access an edge attribute via `$`
g$weight
#> [1] 1 2 5 3
```
