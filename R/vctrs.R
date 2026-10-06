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
  vctrs::new_data_frame(list(x = vctrs::vec_proxy_equal(vals), agg = agg), n = length(x))
}

vec_proxy_compare.agg_vec <- function(x, ...) {
  # `agg` first, so `<aggregated>` sorts after every disaggregated value.
  vctrs::new_data_frame(
    list(agg = agg_vec_is_agg(x), x = vctrs::vec_proxy_compare(agg_vec_expand(x))),
    n = length(x)
  )
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
# The `id` (rather than the ref's memory address, as vecvec uses) separates
# two inputs that are the same object, so `vec_c(x, x)` is a disjoint union
# like `c(x, x)` rather than a replicating slice like `x[c(1:n, 1:n)]`.

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

# Rebuilds the object by slicing each source by its rows' positions (`[`
# reindexes its edges), combining the slices with c() (a disjoint union),
# then reordering the result back into row order. Rows with no source (from
# vec_init()) are sliced from `to` with an NA position.
vec_restore_graph_ref <- function(x, to) {
  ids <- x$id
  n <- length(ids)
  if (n == 0L) {
    return(to[integer()])
  }

  key <- unique(ids)
  if (length(key) == 1L) {
    return(graph_ref_source(x$ref[[1L]], to)[x$i])
  }

  loc <- split(seq_len(n), factor(match(ids, key), levels = seq_along(key)))
  parts <- lapply(loc, function(rows) {
    graph_ref_source(x$ref[[rows[[1L]]]], to)[x$i[rows]]
  })
  out <- do.call(c, unname(parts))

  # `out` holds each group's rows in turn; scatter them back to row order.
  out[order(unlist(loc, use.names = FALSE))]
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
  vec_restore_graph_ref(x, to)
}

# Value-based, like unique.node_vec(): the proxy's refs and positions would
# make every row distinct.
vec_proxy_equal.node_vec <- function(x, ...) {
  vctrs::vec_proxy_equal(node_vec_data(x))
}

vec_ptype_abbr.node_vec <- function(x, ...) {
  paste0("N[", vctrs::vec_ptype_abbr(node_vec_data(x)), "]")
}

vec_ptype2.node_vec.node_vec <- function(x, y, ...) {
  check_same_directed(x, y, ...)
  new_node_vec(
    x = vctrs::vec_ptype2(node_vec_data(x), node_vec_data(y), ...),
    edges = attr(x[integer()], "edges"),
    directed = attr(x, "directed")
  )
}

vec_cast.node_vec.node_vec <- function(x, to, ...) {
  check_same_directed_cast(x, to, ...)
  new_node_vec(
    x = vctrs::vec_cast(node_vec_data(x), node_vec_data(to), ...),
    edges = attr(x, "edges"),
    directed = attr(x, "directed")
  )
}

# -- edge_vec

vec_proxy.edge_vec <- function(x, ...) {
  vec_proxy_graph_ref(x)
}

vec_restore.edge_vec <- function(x, to, ...) {
  vec_restore_graph_ref(x, to)
}

# By the node values at each end and the edge attributes, like node_vec's
# value-based equality. Positions alone would differ between two copies of
# the same edge once vctrs combines them (e.g. in a join), since combining
# offsets them, and would match edges of different graphs.
vec_proxy_equal.edge_vec <- function(x, ...) {
  vctrs::vec_proxy_equal(vctrs::new_data_frame(edge_vec_value_fields(x), n = length(x)))
}

# Ordered by the node values at each end, then the edge attributes. A
# hyperedge role sorts its node sets lexicographically, by rank within `x`,
# so this only orders a single vector (which is all vec_order() needs).
vec_proxy_order.edge_vec <- function(x, ...) {
  fields <- edge_vec_value_fields(x)
  for (role in c("from", "to")) {
    if (is.list(fields[[role]]) && !is.data.frame(fields[[role]])) {
      fields[[role]] <- incidence_set_rank(attr(x, "nodes"), edge_vec_data(x)[[role]])
    }
  }
  vctrs::vec_proxy_order(vctrs::new_data_frame(fields, n = length(x)))
}

# Ranks within one vector can't compare two vectors, so hyperedges have no
# comparison proxy (vec_compare() is the only caller; ordering uses the
# order proxy above).
vec_proxy_compare.edge_vec <- function(x, ...) {
  fields <- edge_vec_data(x)
  if (is.list(fields[["from"]]) || is.list(fields[["to"]])) {
    stop("Can't compare hyperedges with `vec_compare()`; use `vec_order()` to sort them.", call. = FALSE)
  }
  vctrs::vec_proxy_compare(vctrs::new_data_frame(edge_vec_value_fields(x), n = length(x)))
}

# Dense lexicographic rank of each hyperedge's node set, by the order of the
# node values. Shorter sets sort before longer ones sharing their prefix.
incidence_set_rank <- function(nodes, field) {
  node_rank <- if (has_node_values(nodes)) {
    vctrs::vec_rank(nodes, ties = "dense", incomplete = "na")
  } else {
    seq_len(max(c(0L, unlist(field))))
  }
  sets <- lapply(field, function(idx) node_rank[idx])
  width <- max(c(0L, lengths(sets)))
  cols <- lapply(seq_len(width), function(k) {
    vapply(sets, function(s) if (length(s) >= k) s[[k]] else 0L, integer(1))
  })
  if (width == 0L) {
    return(integer(length(field)))
  }
  vctrs::vec_rank(vctrs::new_data_frame(cols, n = length(field)), ties = "dense", incomplete = "na")
}

vec_ptype_abbr.edge_vec <- function(x, ...) {
  nodes <- attr(x, "nodes")
  abbr <- if (is.data.frame(nodes)) "df" else vctrs::vec_ptype_abbr(nodes)
  paste0("E[", abbr, "]")
}

vec_ptype2.edge_vec.edge_vec <- function(x, y, ...) {
  check_same_directed(x, y, ...)
  new_edge_vec_fields(
    fields = edge_vec_data(x[integer()]),
    nodes = vctrs::vec_ptype2(attr(x, "nodes"), attr(y, "nodes"), ...),
    directed = attr(x, "directed")
  )
}

vec_cast.edge_vec.edge_vec <- function(x, to, ...) {
  check_same_directed_cast(x, to, ...)
  new_edge_vec_fields(
    fields = edge_vec_data(x),
    nodes = vctrs::vec_cast(attr(x, "nodes"), vctrs::vec_ptype(attr(to, "nodes")), ...),
    directed = attr(x, "directed")
  )
}
