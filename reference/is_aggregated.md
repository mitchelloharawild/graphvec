# Is the element an aggregation of smaller data

Is the element an aggregation of smaller data

## Usage

``` r
is_aggregated(x)
```

## Arguments

- x:

  An object.

## Value

A logical vector indicating which elements are aggregated.

## See also

[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)

## Examples

``` r
v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
is_aggregated(v)
#> [1]  TRUE FALSE FALSE
```
