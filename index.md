# graphvec

The graphvec package extends vectors to include graph relationships
between their elements, and offers tools to compute useful summaries of
the graph structure for use in summarising, filtering, and otherwise
manipulating the graph.

Nodes are identified by **position**, not by value: each element of the
vector is its own node, and repeated values are distinct nodes that
happen to share a label. Edges are stored as indices into the vector, so
`graphvec` can represent multigraphs, self-loops and isolated nodes
without any special handling.

## Installation

You can install the released version of graphvec from
[CRAN](https://CRAN.R-project.org) with:

``` r

install.packages("graphvec")
```

And the development version from [GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_github("mitchelloharawild/graphvec")
```

## Examples

``` r

library(graphvec)
```

### Nodes

A
[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md)
defines a more general graph structure where nodes can have multiple
parents and children.

``` r

nodes <- node_vec(
  x = factor(c("A", "B", "C", "D", "D", "E")),
  from = c(1L, 3L),
  to = c(2L, 5L)
)

nodes
#> <node_vec[6]>
#> [1] A B C D D E
```

This vector describes a graph of six nodes. The two `D` elements at
positions 4 and 5 are *different* nodes, since node identity comes from
position rather than value, and only position 5 is connected to
anything. The `E` element at position 6 appears in no edge at all, and
remains in the graph as an isolated node.

A `node_vec` wraps its values directly, so it keeps behaving like
whatever it wraps. This one is backed by a factor, and still has levels:

``` r

levels(nodes)
#> [1] "A" "B" "C" "D" "E"
```

Subsetting a `node_vec` selects an induced subgraph: edges that lose an
endpoint are dropped, and the surviving edges are remapped to the new
positions.

``` r

nodes[c(2, 3, 5)]
#> <node_vec[3]>
#> [1] B C D
```

These vectors are particularly useful when used in rectangular tidy data
structures, and slice the same way under dplyr verbs.

``` r

tbl <- dplyr::tibble(nodes, id = 1:6)
dplyr::filter(tbl, id %in% c(2, 3, 5))
#> # A tibble: 3 × 2
#>   nodes       id
#>   <N[fct]> <int>
#> 1 B            2
#> 2 C            3
#> 3 D            5
```

Since node identity is positional, combining two graphs is just a
disjoint union: node vectors concatenate, and the second graph’s edges
shift so they keep pointing at the right nodes.

``` r

g1 <- node_vec(x = c("A", "B"), from = 1L, to = 2L)
g2 <- node_vec(x = c("X", "Y"), from = 1L, to = 2L)

c(g1, g2)
#> <node_vec[4]>
#> [1] A B X Y
```

### Edges

The transpose of a node vector is an
[`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md),
which is instead vectorised along the edges of the graph.

``` r

e <- edge_vec(
  from = c(1L, 2L, 1L, 3L),
  to = c(2L, 3L, 3L, 1L),
  nodes = dplyr::tibble(
    id = 1:3,
    label = c("A", "B", "C")
  )
)

e
#> <edge_vec[4]>
#> [1] [1:A]->[2:B] [2:B]->[3:C] [1:A]->[3:C] [3:C]->[1:A]
```

Values from nodes can be obtained from an edge vector using `$`.

``` r

e$from$label
#> [1] "A" "B" "A" "C"
e$to$label
#> [1] "B" "C" "C" "A"
```

### Aggregated

An
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
is a third graph type built around a common shape in data analysis: a
single parent that aggregates over all the other elements, e.g. a
“Total” row over a set of categories. It marks the aggregate elements
with `<aggregated>` instead of spelling out an edge table.

``` r

av <- agg_vec(
  x = c(NA, "A", "B"),
  aggregated = c(TRUE, FALSE, FALSE)
)

av
#> <agg_vec[3]>
#> [1] <aggregated> A            B
```

An
[`agg_df()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_df.md)
extends this to a table of
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
columns, one row per level of a (possibly crossed) aggregation structure
– e.g. `Purpose` and `State` columns where some rows total one
dimension, some the other, some both.

``` r

kd <- agg_df(
  Purpose = agg_vec(
    c(NA, NA, NA, "Business", "Holiday", "Business", "Business", "Holiday", "Holiday"),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  ),
  State = agg_vec(
    c(NA, "NSW", "VIC", NA, NA, "NSW", "VIC", "NSW", "VIC"),
    c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
)

kd
#> <agg_df[9]>
#> [1] <aggregated>:<aggregated> <aggregated>:NSW         
#> [3] <aggregated>:VIC          Business:<aggregated>    
#> [5] Holiday :<aggregated>     Business:NSW             
#> [7] Business:VIC              Holiday :NSW             
#> [9] Holiday :VIC
```

[`nodes()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)/[`edges()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)
reorient an
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
or
[`agg_df()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_df.md)
into the same
[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md)/[`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md)
graphs seen above: a row is a child of another row whenever the parent
aggregates exactly one more column and matches on every other column’s
disaggregated value.

``` r

nodes(kd)
#> <node_vec[9]>
#> [1] <aggregated>:<aggregated> <aggregated>:NSW         
#> [3] <aggregated>:VIC          Business:<aggregated>    
#> [5] Holiday :<aggregated>     Business:NSW             
#> [7] Business:VIC              Holiday :NSW             
#> [9] Holiday :VIC
edges(kd)
#> <edge_vec[12]>
#>  [1] [Business:<aggregated>]->[<aggregated>:<aggregated>]
#>  [2] [Holiday :<aggregated>]->[<aggregated>:<aggregated>]
#>  [3] [Business:NSW]->[<aggregated>:NSW]                  
#>  [4] [Business:VIC]->[<aggregated>:VIC]                  
#>  [5] [Holiday :NSW]->[<aggregated>:NSW]                  
#>  [6] [Holiday :VIC]->[<aggregated>:VIC]                  
#>  [7] [<aggregated>:NSW]->[<aggregated>:<aggregated>]     
#>  [8] [<aggregated>:VIC]->[<aggregated>:<aggregated>]     
#>  [9] [Business:NSW]->[Business:<aggregated>]             
#> [10] [Business:VIC]->[Business:<aggregated>]             
#> [11] [Holiday :NSW]->[Holiday :<aggregated>]             
#> [12] [Holiday :VIC]->[Holiday :<aggregated>]
```

### igraph

[`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md),
[`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md),
[`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
and
[`agg_df()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_df.md)
can all be converted directly to igraph objects for further analysis.
The vertex count is taken from the nodes rather than inferred from the
edges, so isolated nodes are preserved. Direct vectorised statistics and
operations on these vectors are planned for this package in future
releases.

``` r

igraph::as.igraph(nodes)
#> IGRAPH 6d4f3aa D--- 6 2 -- 
#> + edges from 6d4f3aa:
#> [1] 1->2 3->5
igraph::as.igraph(e)
#> IGRAPH d52a021 D--- 3 4 -- 
#> + edges from d52a021:
#> [1] 1->2 2->3 1->3 3->1
igraph::as.igraph(av)
#> IGRAPH e30398f D--- 3 2 -- 
#> + edges from e30398f:
#> [1] 2->1 3->1
igraph::as.igraph(kd)
#> IGRAPH 0ff9d5d D--- 9 12 -- 
#> + edges from 0ff9d5d:
#>  [1] 4->1 5->1 6->2 7->3 8->2 9->3 2->1 3->1 6->4 7->4 8->5 9->5
```
