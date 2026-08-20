# Create an aggregation table

An `agg_df` is a table of
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
columns, one row per level of a (possibly crossed) aggregation structure
– e.g. `Purpose` and `State` columns where some rows total one
dimension, some the other, some both.
[`nodes()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)/[`edges()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)
reorient it into a graph: a row is a child of another row whenever the
parent aggregates exactly one more column and matches on every other
column's disaggregated value.

## Usage

``` r
agg_df(...)
```

## Arguments

- ...:

  Named `agg_vec` columns, all the same length.

## Value

An `agg_df` object.

## Examples

``` r
agg_df(
  Purpose = agg_vec(c(NA, "Business", "Holiday"), c(TRUE, FALSE, FALSE)),
  State = agg_vec(c("NSW", NA, NA), c(FALSE, TRUE, TRUE))
)
#> <agg_df[3]>
#> [1] <aggregated>:NSW      Business:<aggregated> Holiday :<aggregated>
```
