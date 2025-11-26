# Graph vector along edges

An `edge_vec` is a vector of graph edges with associated node data
stored as attributes.

## Usage

``` r
edge_vec(from = integer(), to = integer(), nodes = data.frame())
```

## Arguments

- from:

  Integer vector of 'from' node indices.

- to:

  Integer vector of 'to' node indices.

- nodes:

  Vector of node data. The vector size should be at least the maximum
  value in `from` and `to`.

## Examples

``` r
g <- edge_vec(
  from = c(1L, 2L, 1L, 3L),
  to = c(2L, 3L, 3L, 1L),
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

```
