# Constructor function for edge_vec

Constructor function for edge_vec

## Usage

``` r
new_edge_vec(
  from = integer(),
  to = integer(),
  ...,
  nodes = data.frame(),
  directed = TRUE
)
```

## Arguments

- from:

  Integer vector of 'from' node indices.

- to:

  Integer vector of 'to' node indices.

- ...:

  Named edge attribute fields, already recycled to the number of edges.

- nodes:

  Vector of node data (any vector, including a data frame of node
  attributes).

- directed:

  A single logical value: is incidence ordered (`from` -\> `to`) or
  symmetric?

## Value

An `edge_vec` object.
