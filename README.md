

<!-- README.md is generated from README.qmd. Please edit that file -->

# graphvec <a href="https://pkg.mitchelloharawild.com/graphvec"><img src="man/figures/logo.svg" align="right" height="139" alt="graphvec website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/graphvec.png)](https://CRAN.R-project.org/package=graphvec)
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
adjacency list.

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
g <- graph_vec(
  x = factor(c("A", "B", "C", "D", "D", "E")),
  g = tibble(
    from = list(c(1L, 3L), c(2L, 4L)),
    to = c(2L, 5L)
  )
)

g
#> [1] A B C D D E
#> attr(,"g")
#> # A tibble: 2 × 2
#>          from    to
#>   <list<int>> <int>
#> 1         [2]     2
#> 2         [2]     5
#> Levels: A B C D E
node_is_child(g, "B")
#> [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE
```

These vectors are particularly useful when used in rectangular tidy data
structures.

``` r
x <- tibble(g, y = rnorm(6))
x
#> # A tibble: 6 × 2
#>   g          y
#>   <fct>  <dbl>
#> 1 A     -0.533
#> 2 B     -0.623
#> 3 C      0.659
#> 4 D      2.45 
#> 5 D      0.551
#> 6 E     -0.699
x |> 
  filter(node_is_child(g, "B"))
#> # A tibble: 3 × 2
#>   g          y
#>   <fct>  <dbl>
#> 1 A     -0.533
#> 2 B     -0.623
#> 3 C      0.659
x |> 
  mutate(node_degree(g))
#> # A tibble: 6 × 3
#>   g          y `node_degree(g)`
#>   <fct>  <dbl>            <dbl>
#> 1 A     -0.533                1
#> 2 B     -0.623                3
#> 3 C      0.659                1
#> 4 D      2.45                 1
#> 5 D      0.551                1
#> 6 E     -0.699                2
```
