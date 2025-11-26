# Internal vctrs methods

These methods are the extensions that allow aggregation vectors to work
with vctrs.

## Usage

``` r
# S3 method for class 'agg_vec'
vec_ptype2(x, y, ...)

# S3 method for class 'agg_vec.agg_vec'
vec_ptype2(x, y, ...)

# S3 method for class 'agg_vec.default'
vec_ptype2(x, y, ...)

# S3 method for class 'agg_vec.character'
vec_ptype2(x, y, ...)

# S3 method for class 'character.agg_vec'
vec_ptype2(x, y, ...)

# S3 method for class 'agg_vec'
vec_ptype_abbr(x, ...)

# S3 method for class 'agg_vec'
vec_cast(x, to, ...)

# S3 method for class 'agg_vec.agg_vec'
vec_cast(x, to, ...)

# S3 method for class 'agg_vec.default'
vec_cast(x, to, ...)

# S3 method for class 'character.agg_vec'
vec_cast(x, to, ...)

# S3 method for class 'agg_vec'
vec_proxy_compare(x, ...)

# S3 method for class 'edge_vec'
vec_ptype2(x, y, ...)

# S3 method for class 'edge_vec.default'
vec_ptype2(x, y, ...)

# S3 method for class 'edge_vec.character'
vec_ptype2(x, y, ...)

# S3 method for class 'edge_vec'
vec_cast(x, to, ...)

# S3 method for class 'edge_vec.edge_vec'
vec_cast(x, to, ...)

# S3 method for class 'edge_vec.default'
vec_cast(x, to, ...)

# S3 method for class 'character.edge_vec'
vec_cast(x, to, ...)

# S3 method for class 'edge_vec'
vec_ptype_abbr(x, ...)

# S3 method for class 'node_vec'
vec_ptype2(x, y, ...)

# S3 method for class 'node_vec.default'
vec_ptype2(x, y, ...)

# S3 method for class 'node_vec.character'
vec_ptype2(x, y, ...)

# S3 method for class 'node_vec'
vec_cast(x, to, ...)

# S3 method for class 'node_vec.node_vec'
vec_cast(x, to, ...)

# S3 method for class 'node_vec.default'
vec_cast(x, to, ...)

# S3 method for class 'character.node_vec'
vec_cast(x, to, ...)

# S3 method for class 'node_vec.character'
vec_cast(x, to, ...)

# S3 method for class 'node_vec'
vec_ptype_abbr(x, ...)
```
