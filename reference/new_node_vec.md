# Constructor function for node_vec

Constructor function for node_vec

## Usage

``` r
new_node_vec(
  x = list(),
  edges = data.frame(from = integer(), to = integer()),
  directed = TRUE
)
```

## Arguments

- x:

  A vector representing the nodes in the graph.

- edges:

  A data frame with columns `from` and `to` representing the edges

- directed:

  A single logical value: is incidence ordered (`from` -\> `to`) or
  symmetric?

## Value

A `node_vec` object.
