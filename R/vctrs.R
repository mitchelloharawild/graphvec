# vctrs compatibility methods
#
# vctrs is only suggested, so none of these are exported: they're registered
# dynamically by register_vctrs_methods() (via zzz.R) whenever vctrs is
# loaded, and may call vctrs:: freely since they can only run once it is.

# Base vector classes an agg_vec's values can be combined/cast with, in
# either order. Hard-coded rather than read from vctrs' exports so that
# loading graphvec doesn't also load vctrs.
agg_vec_vctrs_types <- c(
  "logical", "integer", "double", "complex", "character", "raw",
  "factor", "ordered", "Date", "POSIXct", "POSIXlt", "difftime"
)

# nocov start
register_vctrs_methods <- function() {
  for (cls in c("agg_vec", "node_vec", "edge_vec")) {
    register_s3_method("vctrs", "vec_proxy", cls)
    register_s3_method("vctrs", "vec_restore", cls)
    register_s3_method("vctrs", "vec_proxy_equal", cls)
    register_s3_method("vctrs", "vec_ptype_abbr", cls)
    # Same-class methods stop vctrs falling back to c() (see _dev/vector.md §8).
    register_s3_method("vctrs", "vec_ptype2", paste0(cls, ".", cls))
    register_s3_method("vctrs", "vec_cast", paste0(cls, ".", cls))
  }
  register_s3_method("vctrs", "vec_proxy_compare", "agg_vec")
  register_s3_method("vctrs", "vec_proxy_compare", "node_vec")
  register_s3_method("vctrs", "vec_proxy_order", "node_vec")
  register_s3_method("vctrs", "vec_proxy_compare", "edge_vec")
  register_s3_method("vctrs", "vec_proxy_order", "edge_vec")

  for (type in agg_vec_vctrs_types) {
    register_s3_method("vctrs", "vec_ptype2", paste0("agg_vec.", type), fun = vec_ptype2_agg_vec_other)
    register_s3_method("vctrs", "vec_ptype2", paste0(type, ".agg_vec"), fun = vec_ptype2_other_agg_vec)
    register_s3_method("vctrs", "vec_cast", paste0("agg_vec.", type), fun = vec_cast_to_agg_vec)
    register_s3_method("vctrs", "vec_cast", paste0(type, ".agg_vec"), fun = vec_cast_from_agg_vec)
  }
}
# nocov end

# -- agg_vec -------------------------------------------------------------
#
# Nothing is shared between rows, so the proxy is just the expanded form:
# one row per element, its value (NA if aggregated) and its aggregated flag.

vec_proxy.agg_vec <- function(x, ...) {
  vctrs::new_data_frame(
    list(x = agg_vec_expand(x), agg = agg_vec_is_agg(x)),
    n = length(x)
  )
}

vec_restore.agg_vec <- function(x, to, ...) {
  # vec_init() fills `agg` with NA: an unaggregated missing value.
  is_agg <- x$agg %in% TRUE
  new_agg_vec(vctrs::vec_slice(x$x, !is_agg), which(is_agg))
}

vec_proxy_equal.agg_vec <- function(x, ...) {
  vals <- agg_vec_expand(x)
  is_agg <- agg_vec_is_agg(x)
  # A missing disaggregated value has NA in both columns, so vctrs (which
  # only counts a row as missing when every column is) sees it as missing.
  agg <- is_agg
  agg[!is_agg & vctrs::vec_detect_missing(vals)] <- NA
  # `<aggregated>` has no value, so give it the same non-missing filler in
  # every agg_vec. vctrs then sees it as complete (e.g. drop_na() keeps it)
  # and as unequal, not NA, to any value; `agg` alone tells them apart.
  proxy <- fill_proxy(vctrs::vec_proxy_equal(vals), is_agg)
  vctrs::new_data_frame(list(x = proxy, agg = agg), n = length(x))
}

# Sets the rows `where` of an equality proxy (an atomic vector, a data frame
# of them, or a list) to a fixed non-missing value of its type. Classes such
# as Date or factor are dropped first: the proxy only needs to compare.
fill_proxy <- function(x, where) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, fill_proxy, where = where)
    return(x)
  }
  if (is.list(x)) {
    x[where] <- list(FALSE)
    return(x)
  }
  x <- vctrs::vec_data(x)
  x[where] <- switch(typeof(x),
    logical = FALSE,
    integer = 0L,
    double = 0,
    complex = 0i,
    character = "",
    raw = as.raw(0L)
  )
  x
}

vec_proxy_compare.agg_vec <- function(x, ...) {
  vals <- agg_vec_expand(x)
  is_agg <- agg_vec_is_agg(x)
  # As for vec_proxy_equal.agg_vec(): a missing disaggregated value is NA
  # throughout, so it compares as NA and sorts with other missing values,
  # while `<aggregated>` gets a filler value so it compares equal to itself.
  # `agg` comes first, so `<aggregated>` sorts after every value.
  agg <- is_agg
  agg[!is_agg & vctrs::vec_detect_missing(vals)] <- NA
  proxy <- fill_proxy(vctrs::vec_proxy_compare(vals), is_agg)
  vctrs::new_data_frame(list(agg = agg, x = proxy), n = length(x))
}

vec_ptype_abbr.agg_vec <- function(x, ...) {
  paste0(vctrs::vec_ptype_abbr(agg_vec_values(x)), "*")
}

# The value ptype of an agg_vec, for vec_ptype2(). A bare logical one is
# treated as unspecified, so an all-`<aggregated>` agg_vec (whose values
# default to logical, e.g. `agg_vec(NA, TRUE)`) takes on the other side's
# value type.
agg_vec_values_ptype2 <- function(x) {
  vals <- vctrs::vec_ptype(agg_vec_values(x))
  if (is.logical(vals) && !is.object(vals)) vctrs::unspecified() else vals
}

new_agg_vec_ptype <- function(vals) {
  if (inherits(vals, "vctrs_unspecified")) vals <- logical()
  new_agg_vec(vals, integer())
}

# Casts an agg_vec's values (or any vector) to the value type `to`, letting
# all-missing bare logical values (including none at all) take on any type,
# per agg_vec_values_ptype2().
agg_vec_cast_values <- function(vals, to, ...) {
  if (is.logical(vals) && !is.object(vals) && all(is.na(vals))) {
    return(vctrs::vec_init(to, length(vals)))
  }
  vctrs::vec_cast(vals, to, ...)
}

vec_ptype2.agg_vec.agg_vec <- function(x, y, ...) {
  new_agg_vec_ptype(vctrs::vec_ptype2(agg_vec_values_ptype2(x), agg_vec_values_ptype2(y), ...))
}

vec_ptype2_agg_vec_other <- function(x, y, ...) {
  new_agg_vec_ptype(vctrs::vec_ptype2(agg_vec_values_ptype2(x), y, ...))
}

vec_ptype2_other_agg_vec <- function(x, y, ...) {
  new_agg_vec_ptype(vctrs::vec_ptype2(x, agg_vec_values_ptype2(y), ...))
}

vec_cast.agg_vec.agg_vec <- function(x, to, ...) {
  new_agg_vec(agg_vec_cast_values(agg_vec_values(x), agg_vec_values(to), ...), attr(x, "agg_pos"))
}

vec_cast_to_agg_vec <- function(x, to, ...) {
  new_agg_vec(vctrs::vec_cast(x, agg_vec_values(to), ...), integer())
}

# Lossy wherever x is `<aggregated>`, which has no value in a plain vector.
vec_cast_from_agg_vec <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vctrs::vec_cast(agg_vec_expand(x), to, ..., x_arg = x_arg, to_arg = to_arg)
  vctrs::maybe_lossy_cast(
    out, x, to,
    lossy = agg_vec_is_agg(x),
    x_arg = x_arg, to_arg = to_arg,
    details = "`<aggregated>` values can't be represented without an `agg_vec`."
  )
}

# -- node_vec and edge_vec -----------------------------------------------
#
# Each row of the proxy points to the whole object it came from (`ref`), its
# position in it (`i`), and an `id` unique to each vec_proxy() call (see
# _dev/vector.md §11). vctrs only ever moves proxy rows around, so every row
# keeps track of its own graph through slicing, vec_init(), assignment and
# combining; nothing that depends on the rows has to survive as an attribute,
# which vec_rbind() would reset (_dev/vector.md §7-§8).
#
# The restore puts rows back together by *graph*, not by proxy call: a ref
# carries its graph (the `graph` external pointer, kept by every slice), and
# rows of the same graph go back into one graph whichever call they came
# through, so `if_else(cond, x, x)`, `vec_assign(x, i, x[i])` and a no-op
# rows_patch() keep every edge (_dev/graph-identity.md). The `id` still
# matters: it says which *input* a row came from, which is what tells
# `vec_c(x, x)` (the same nodes from two inputs: a disjoint union, like
# `c(x, x)`) apart from `vec_slice(x, c(1:n, 1:n))` (one input replicating
# its nodes). node_vec_assemble() and c.edge_vec() hold the exact rules.

graph_ref_counter <- new.env(parent = emptyenv())
graph_ref_counter$id <- 0

vec_proxy_graph_ref <- function(x) {
  n <- length(x)
  graph_ref_counter$id <- graph_ref_counter$id + 1
  vctrs::new_data_frame(
    list(ref = rep(list(x), n), id = rep(graph_ref_counter$id, n), i = seq_len(n)),
    n = n
  )
}

# One input per proxy call (`id`), the object behind it, and each row's
# position in it, handed to `combine(srcs, src, row)`. Rows with no source
# (from vec_init(), which leaves `ref`, `id` and `i` missing) are taken from
# `to` with an NA position: a missing node/edge.
vec_restore_graph_ref <- function(x, to, combine) {
  ids <- x$id
  n <- length(ids)
  if (n == 0L) {
    return(to[integer()])
  }
  key <- unique(ids)
  src <- match(ids, key)
  srcs <- lapply(match(key, ids), function(r) graph_ref_source(x$ref[[r]], to))
  combine(srcs, src, x$i)
}

graph_ref_source <- function(ref, to) {
  if (is.null(ref)) to else ref
}


# vctrs' incompatible-type error for two node_vecs/edge_vecs that differ in
# `directed`, which c() refuses to combine.
check_same_directed <- function(x, y, x_arg = "", y_arg = "", ...) {
  if (!identical(attr(x, "directed"), attr(y, "directed"))) {
    vctrs::stop_incompatible_type(
      x, y,
      x_arg = x_arg, y_arg = y_arg,
      details = "They have different `directed`."
    )
  }
}

check_same_directed_cast <- function(x, to, x_arg = "", to_arg = "", ...) {
  if (!identical(attr(x, "directed"), attr(to, "directed"))) {
    vctrs::stop_incompatible_cast(
      x, to,
      x_arg = x_arg, to_arg = to_arg,
      details = "They have different `directed`."
    )
  }
}

# -- node_vec

vec_proxy.node_vec <- function(x, ...) {
  vec_proxy_graph_ref(x)
}

vec_restore.node_vec <- function(x, to, ...) {
  vec_restore_graph_ref(x, to, node_vec_assemble)
}

# Identity (graph + position, kept through copies) + value: two nodes are
# equal exactly when they are (copies of) the same node of the same graph
# with the same value (node_vec_equal_fields()). Nodes of different graphs
# never match, whatever their labels; match those by label explicitly, e.g.
# on format(n). Joins combine needles and haystack before comparing, which
# makes disjoint-union copies of nodes on both sides; the copies keep their
# origin, so they still match.
vec_proxy_equal.node_vec <- function(x, ...) {
  vctrs::vec_proxy_equal(vctrs::new_data_frame(node_vec_equal_fields(x), n = length(x)))
}

# Ordered by value, then by identity as a tie-break, so only equal nodes tie.
# Same fields as xtfrm.node_vec(), so base and vctrs sorting agree.
vec_proxy_order.node_vec <- function(x, ...) {
  vctrs::vec_proxy_order(vctrs::new_data_frame(node_vec_order_fields(x), n = length(x)))
}

vec_proxy_compare.node_vec <- function(x, ...) {
  vctrs::vec_proxy_compare(vctrs::new_data_frame(node_vec_order_fields(x), n = length(x)))
}

vec_ptype_abbr.node_vec <- function(x, ...) {
  paste0("N[", vctrs::vec_ptype_abbr(node_vec_data(x)), "]")
}

vec_ptype2.node_vec.node_vec <- function(x, y, ...) {
  check_same_directed(x, y, ...)
  node_vec_with_values(x[integer()], vctrs::vec_ptype2(node_vec_data(x), node_vec_data(y), ...))
}

vec_cast.node_vec.node_vec <- function(x, to, ...) {
  check_same_directed_cast(x, to, ...)
  # Casting only changes the node values' type, never the graph, so a cast
  # node_vec still combines with the rest of its graph (vctrs casts every
  # input to the common type before combining them).
  node_vec_with_values(x, vctrs::vec_cast(node_vec_data(x), node_vec_data(to), ...))
}

# -- edge_vec

vec_proxy.edge_vec <- function(x, ...) {
  vec_proxy_graph_ref(x)
}

vec_restore.edge_vec <- function(x, to, ...) {
  vec_restore_graph_ref(x, to, edge_vec_assemble)
}

# Graph + positions (+ edge attributes): two edges are equal exactly when
# they are the same graph's edges between the same nodes, with or without
# node data (edge_vec_equal_fields()). Edges of different graphs never
# match, whatever their labels; match those by label explicitly, e.g. on
# format(e). Comparing within a combined vector (joins combine needles and
# haystack first) works because c() shares the graph between inputs of the
# same graph instead of offsetting their positions.
vec_proxy_equal.edge_vec <- function(x, ...) {
  vctrs::vec_proxy_equal(vctrs::new_data_frame(edge_vec_equal_fields(x), n = length(x)))
}

# Ordered by the node values at each end, then the edge attributes, then
# (ordinary edges) the node positions as a tie-break, so only equal edges
# tie. A hyperedge role sorts its node sets lexicographically, by rank
# within `x`, so this only orders a single vector (which is all vec_order()
# needs). Same fields as xtfrm.edge_vec(), so base and vctrs sorting agree.
vec_proxy_order.edge_vec <- function(x, ...) {
  fields <- edge_vec_order_fields(x)
  vctrs::vec_proxy_order(vctrs::new_data_frame(fields, n = length(x)))
}

# Ranks within one vector can't compare two vectors, so hyperedges have no
# comparison proxy (vec_compare() is the only caller; ordering uses the
# order proxy above).
vec_proxy_compare.edge_vec <- function(x, ...) {
  ends <- edge_vec_endpoints(x)
  if (is.list(ends$from) || is.list(ends$to)) {
    stop("Can't compare hyperedges with `vec_compare()`; use `vec_order()` to sort them.", call. = FALSE)
  }
  # As the order proxy, then the graph, so edges of different graphs never
  # compare as equal either.
  fields <- edge_vec_order_fields(x)
  uid <- rep(attr(x, "graph")$uid(), length(x))
  uid[is.na(attr(x, "edge_id"))] <- NA
  vctrs::vec_proxy_compare(vctrs::new_data_frame(c(fields, list(.graph = uid)), n = length(x)))
}

vec_ptype_abbr.edge_vec <- function(x, ...) {
  nodes <- attr(x, "nodes")
  abbr <- if (is.data.frame(nodes)) "df" else vctrs::vec_ptype_abbr(nodes)
  paste0("E[", abbr, "]")
}

vec_ptype2.edge_vec.edge_vec <- function(x, y, ...) {
  check_same_directed(x, y, ...)
  new_edge_vec_fields(
    fields = as.list(edge_vec_fields_df(x[integer()])),
    nodes = vctrs::vec_ptype2(attr(x, "nodes"), attr(y, "nodes"), ...),
    directed = attr(x, "directed")
  )
}

vec_cast.edge_vec.edge_vec <- function(x, to, ...) {
  check_same_directed_cast(x, to, ...)
  graph <- attr(x, "graph")
  if (!is.null(graph)) {
    # Only the node values' type changes, never the graph, so a cast edge_vec
    # still shares its graph with the rest of it when combined (vctrs casts
    # every input to the common type first; same-graph inputs get identical
    # node tables back, which c.edge_vec() requires to share).
    nodes <- attr(x, "nodes")
    cast <- vctrs::vec_cast(nodes, vctrs::vec_ptype(attr(to, "nodes")), ...)
    if (identical(cast, nodes)) {
      return(x)
    }
    attr(x, "nodes") <- cast
    return(x)
  }
  new_edge_vec_fields(
    fields = as.list(edge_vec_fields_df(x)),
    nodes = vctrs::vec_cast(attr(x, "nodes"), vctrs::vec_ptype(attr(to, "nodes")), ...),
    directed = attr(x, "directed")
  )
}
