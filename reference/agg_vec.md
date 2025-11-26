# Create an aggregation vector

maturing

## Usage

``` r
agg_vec(x = character(), aggregated = logical(vec_size(x)))
```

## Arguments

- x:

  The vector of values.

- aggregated:

  A logical vector to identify which values are `<aggregated>`.

## Details

An aggregation vector is a special type of
[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md)
consisting of a single parent (the 'aggregated' value) and its children.
Aggregated values are identified by a logical vector passed to the
`aggregated` argument, and disaggregated values are provided in `x`.
Aggregated values are displayed as `<aggregated>` by default.

## Examples

``` r
agg_vec(
  x = c(NA, "A", "B"),
  aggregated = c(TRUE, FALSE, FALSE)
)
#> <agg_vec[3]>
#> [1] <aggregated> A            B           
```
