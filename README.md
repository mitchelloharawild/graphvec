
<!-- README.md is generated from README.Rmd. Please edit that file -->

# graphvec

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/graphvec)](https://CRAN.R-project.org/package=graphvec)
<!-- badges: end -->

The graphvec package extends vectors to include graph relationships
between their unique values, and offers tools to compute useful
summaries of the graph structure for use in summarising, filtering, and
otherwise manipulating the graph.

## Installation

You can install the development version of graphvec from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mitchelloharawild/graphvec")
```

## Examples

An `agg_vec()` (aggregation vector) defines a simple hierarchical graph
structure where there is a single parent node.

``` r
library(graphvec)
agg_vec(
  x = c(NA, "A", "B"),
  aggregated = c(TRUE, FALSE, FALSE)
)
#> <agg_vec[3]>
#> [1] <aggregated> A            B
```

A `graph_vec()` (graph vector) defines a general graph structure where
the relationships among the levels of a factor are specified in an
edge-linked graph.

``` r
g <- graph_vec(
  x = factor(c("A", "B", "C", "D", "D", "E")),
  edges = matrix(
    c(
      1L, 2L,
      3L, 2L,
      2L, 5L,
      4L, 5L
    ), 
    ncol = 2, byrow = TRUE,
    dimnames = list(c(), c("from", "to"))
  )
)

g
#> <graph_vec[6]>
#> [1] A B C D D E
node_is_child(g, "B")
#> [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE
```

These vectors are particularly useful when used in rectangular tidy data
structures.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
x <- tibble(g, y = rnorm(6))
x
#> # A tibble: 6 x 2
#>         g      y
#>   <graph>  <dbl>
#> 1       A -0.381
#> 2       B -0.344
#> 3       C  0.341
#> 4       D  0.783
#> 5       D -0.801
#> 6       E -0.462
x |> 
  filter(node_is_child(g, "B"))
#> # A tibble: 3 x 2
#>         g      y
#>   <graph>  <dbl>
#> 1       A -0.381
#> 2       B -0.344
#> 3       C  0.341
x |> 
  mutate(node_degree(g))
#> # A tibble: 6 x 3
#>         g      y `node_degree(g)`
#>   <graph>  <dbl>            <dbl>
#> 1       A -0.381                1
#> 2       B -0.344                3
#> 3       C  0.341                1
#> 4       D  0.783                1
#> 5       D -0.801                1
#> 6       E -0.462                2
```
