# Package index

## Graph vectors

Vectorised graph data structures.

- [`node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/node_vec.md)
  : Graph vector along nodes
- [`edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/edge_vec.md)
  : Graph vector along edges
- [`agg_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_vec.md)
  : Create an aggregation vector
- [`agg_df()`](https://pkg.mitchelloharawild.com/graphvec/reference/agg_df.md)
  : Create an aggregation table

## Graph structure

Access the nodes and edges of a graph vector, and identify aggregated
elements.

- [`nodes()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)
  [`edges()`](https://pkg.mitchelloharawild.com/graphvec/reference/reorient.md)
  : Reorient a graph vector
- [`is_aggregated()`](https://pkg.mitchelloharawild.com/graphvec/reference/is_aggregated.md)
  : Is the element an aggregation of smaller data

## Interoperability

Convert graph vectors to other graph representations.

- [`as.igraph(`*`<agg_vec>`*`)`](https://pkg.mitchelloharawild.com/graphvec/reference/as.igraph.md)
  [`as.igraph(`*`<agg_df>`*`)`](https://pkg.mitchelloharawild.com/graphvec/reference/as.igraph.md)
  [`as.igraph(`*`<node_vec>`*`)`](https://pkg.mitchelloharawild.com/graphvec/reference/as.igraph.md)
  [`as.igraph(`*`<edge_vec>`*`)`](https://pkg.mitchelloharawild.com/graphvec/reference/as.igraph.md)
  : Convert a graph vector to an igraph object

## Developer tools

Low-level constructors for extending graphvec’s classes.

- [`new_node_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/new_node_vec.md)
  : Constructor function for node_vec
- [`new_edge_vec()`](https://pkg.mitchelloharawild.com/graphvec/reference/new_edge_vec.md)
  : Constructor function for edge_vec
