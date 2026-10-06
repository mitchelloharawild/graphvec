#' Graph vector along edges
#'
#' An `edge_vec` is a vector of graph edges with associated node data stored as
#' attributes.
#'
#' @param from Integer vector of 'from' node indices, one per edge. A list of
#' integer vectors instead opts into hyperedges: each element gives the
#' (zero or more) 'from' node positions for that edge, an ordinary edge
#' being the length-1 case.
#' @param to Integer vector of 'to' node indices, or a list of integer
#' vectors for hyperedges, the same way as `from`.
#' @param ... Named edge attribute vectors (e.g. `weight = c(1, 2, 5)`),
#' recycled to the number of edges. `from` and `to` are reserved and cannot
#' be used as attribute names. Attribute columns are stored on the edge table
#' itself, so they slice, replicate, and reorient with the edges they belong
#' to (see [nodes()]/[edges()]).
#' @param nodes Vector of node data (any vector, including a data
#' frame of node attributes). Its size should be at least the maximum value
#' in `from` and `to`.
#' @param directed A single logical value: is incidence ordered (`from` -> `to`)
#' or symmetric?
#'
#' @return An `edge_vec` object.
#'
#' @examples
#' g <- edge_vec(
#'   from = c(1L, 2L, 1L, 3L),
#'   to = c(2L, 3L, 3L, 1L),
#'   weight = c(1, 2, 5, 3),
#'   nodes = data.frame(
#'     id = 1:3,
#'     label = c("A", "B", "C")
#'   )
#' )
#'
#' # Access node data via `$`
#' g$from$label
#' g$to$label
#'
#' # Access an edge attribute via `$`
#' g$weight
#'
#' @export
edge_vec <- function(from = integer(), to = integer(), ..., nodes = data.frame(), directed = TRUE) {
  fields <- new_edge_attrs(from, to, ...)

  stopifnot(is_valid_incidence(fields$from))
  stopifnot(is_valid_incidence(fields$to))
  stopifnot(is.atomic(nodes) || is.list(nodes))
  stopifnot(is.logical(directed), length(directed) == 1)
  stopifnot(!is.na(directed))

  do.call(new_edge_vec, c(fields, list(nodes = nodes, directed = directed)))
}

# Validates and recycles `from`/`to` alongside `...` edge attributes to a
# common length; shared by node_vec() and edge_vec().
new_edge_attrs <- function(from, to, ...) {
  attrs <- list(...)
  if (length(attrs) > 0 && (is.null(names(attrs)) || any(names(attrs) == ""))) {
    stop("All edge attributes passed via `...` must be named.", call. = FALSE)
  }
  # Recycles each field up to the others' length and errors on mismatch, same
  # as data.frame() -- but also handles a hyperedge (list) `from`/`to` column,
  # which data.frame() refuses to recycle against a plain one.
  cols <- recycle_common(c(list(from = from, to = to), attrs))
  as.list(edge_fields_df(cols))
}

# Drops "edge_vec" from x's class and clears the nodes/directed/graph/edge_id
# attributes, leaving the plain attribute fields as an unclassed named list
# (ordinary case) or the plain from/to/attrs fields (hyperedge case).
strip_edge_vec <- function(x) {
  attr(x, "nodes") <- NULL
  attr(x, "directed") <- NULL
  attr(x, "graph") <- NULL # absent (NULL already) for the hyperedge case
  attr(x, "edge_id") <- NULL
  cls <- setdiff(oldClass(x), "edge_vec")
  oldClass(x) <- NULL
  if (!identical(cls, class(x))) oldClass(x) <- cls
  x
}

edge_vec_data <- strip_edge_vec

# The current from/to positions, in x's current row order -- derived from
# `graph` for the ordinary case (never stored a second time), or read
# directly off the fields for the hyperedge case.
edge_vec_endpoints <- function(x) {
  graph <- attr(x, "graph")
  if (is.null(graph)) {
    fields <- edge_vec_data(x)
    return(list(from = fields[["from"]], to = fields[["to"]]))
  }
  ends <- graph$edge_endpoints()
  id <- attr(x, "edge_id")
  list(from = ends$from[id], to = ends$to[id])
}

# The fields, rewrapped as a genuine data frame with from/to reunited with
# the attribute columns, for call sites that need real row-wise semantics
# ([.data.frame/rbind()/as_tibble()/as.igraph() etc.) or the full from/to/
# attrs shape (c()'s hyperedge up-casting).
edge_vec_fields_df <- function(x) {
  graph <- attr(x, "graph")
  if (is.null(graph)) {
    return(edge_fields_df(edge_vec_data(x)))
  }
  ends <- edge_vec_endpoints(x)
  cbind_edge_fields(ends$from, ends$to, edge_vec_data(x))
}

#' Constructor function for edge_vec
#'
#' @param from Integer vector of 'from' node indices, or a list of integer
#' vectors for hyperedges.
#' @param to Integer vector of 'to' node indices, or a list of integer
#' vectors for hyperedges.
#' @param ... Named edge attribute fields, already recycled to the number of
#' edges.
#' @param nodes Vector of node data (any vector, including a data
#' frame of node attributes).
#' @param directed A single logical value: is incidence ordered (`from` -> `to`)
#' or symmetric?
#' @return An `edge_vec` object.
#'
#' @examples
#' new_edge_vec(
#'   from = c(1L, 2L),
#'   to = c(2L, 3L),
#'   nodes = data.frame(label = c("A", "B", "C"))
#' )
#'
#' @export
new_edge_vec <- function(from = integer(), to = integer(), ..., nodes = data.frame(), directed = TRUE) {
  fields <- new_edge_attrs(from, to, ...)
  new_edge_vec_fields(fields, nodes = nodes, directed = directed)
}

# Low-level constructor from an already-assembled from/to/attrs fields
# (a plain named list); used internally by [.edge_vec/c.edge_vec to skip
# re-recycling.
new_edge_vec_fields <- function(fields, nodes = data.frame(), directed = TRUE) {
  # No node data (a zero-column data frame) still has a node count: every
  # position the edges reference. This is what c() offsets by, so combining
  # edge_vecs without node data is a disjoint union like any other.
  if (is.data.frame(nodes) && ncol(nodes) == 0L) {
    n_nodes <- max(NROW(nodes), unlist(fields[c("from", "to")], use.names = FALSE), 0L, na.rm = TRUE)
    nodes <- data.frame(row.names = seq_len(n_nodes))
  }

  from <- fields[["from"]]
  to <- fields[["to"]]

  # Hyperedges (list-valued from/to) are out of scope for the Rust backend
  # (_dev/RUST_BACKEND.md) -- keep today's from/to-in-the-fields
  # representation exactly as-is, unaccelerated.
  if (is.list(from) || is.list(to)) {
    body <- edge_fields_df(fields)
    attr(body, "row.names") <- NULL # stray leftover once no longer classed data.frame
    return(structure(body, class = "edge_vec", nodes = nodes, directed = directed))
  }

  # Ordinary case: topology moves into a shared GraphBackend; the object's
  # own body keeps only the attribute columns, aligned 1:1 with the graph's
  # edge order.
  n_edges <- length(from)
  # A missing edge (NA at both ends, e.g. from vctrs::vec_init() or an NA
  # index, then combined with c()) has no place in the graph, so it's left
  # out and given an NA edge_id, exactly as an NA index into `[` gives it.
  # The backend can't hold an edge with only one end missing.
  missing_from <- is.na(from)
  missing_to <- is.na(to)
  if (any(missing_from != missing_to)) {
    stop("An edge can't have only one of `from` and `to` missing.", call. = FALSE)
  }
  present <- !missing_from
  graph <- graphvec_backend_new(NROW(nodes), from[present], to[present], directed)
  edge_id <- rep(NA_integer_, n_edges)
  edge_id[present] <- seq_len(sum(present))
  attrs <- attrs_frame(fields[setdiff(names(fields), c("from", "to"))], n_edges)
  new_edge_vec_backend(attrs, nodes = nodes, directed = directed, graph = graph, edge_id = edge_id)
}

# Low-level constructor for the non-hyperedge (Rust-backed) case: `attrs` is
# already the attribute-only table, `graph` is already built, and `edge_id`
# says which of `graph`'s edges (in which order) `attrs`'s rows correspond
# to -- used by new_edge_vec_fields() and by nodes.node_vec()'s free
# (no-Rust-call) reorientation via edges.node_vec().
new_edge_vec_backend <- function(attrs, nodes, directed, graph, edge_id) {
  rownames(attrs) <- NULL # keeps the row count even when attrs has 0 columns
  structure(
    attrs,
    class = "edge_vec",
    nodes = nodes,
    directed = directed,
    graph = graph,
    edge_id = edge_id
  )
}

#' @export
format.edge_vec <- function(x, ...){
  key_data <- attr(x, "nodes")
  ends <- edge_vec_endpoints(x)
  # -- undirected
  # -> directed
  arrow <- if (isTRUE(attr(x, "directed"))) "->" else "--"
  sprintf(
    "[%s]%s[%s]",
    incidence_label(key_data, ends$from),
    arrow,
    incidence_label(key_data, ends$to)
  )
}

# One string per edge, its formatted label; the underlying list would give
# one per field.
#' @export
as.character.edge_vec <- function(x, ...) {
  trimws(format(x, ...))
}

#' @export
print.edge_vec <- function(x, ...) {
  cat(sprintf("<edge_vec[%d]>\n", length(x)))
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

# Registered dynamically for pillar via zzz.R.
pillar_shaft.edge_vec <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format(x, ...), align = "left", min_width = 10)
}

#' Subset an edge_vec
#'
#' Slicing an `edge_vec` selects edges directly: dropping or reordering
#' edges never invalidates a node reference, so `nodes`/`directed` are
#' unaffected -- unlike slicing a [`node_vec()`], no remap is needed.
#'
#' @param x An `edge_vec`.
#' @param i Indices to select, as for `` `[` ``.
#' @param ... Passed on.
#' @return An `edge_vec` containing only the selected edges.
#' @examples
#' g <- edge_vec(
#'   from = c(1L, 2L, 1L, 3L),
#'   to = c(2L, 3L, 3L, 1L),
#'   nodes = data.frame(label = c("A", "B", "C"))
#' )
#' g[1:2]
#' @keywords internal
#' @export
`[.edge_vec` <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }

  idx <- seq_len(length(x))[i]
  graph <- attr(x, "graph")

  if (!is.null(graph)) {
    # No topology remap needed (_dev/RUST_BACKEND.md §2.2): the same graph
    # is carried forward unchanged, only the attribute table and the
    # edge-id positions into `graph`'s edge order are subset. Slicing goes
    # through a genuinely "data.frame"-classed copy (nodes/directed/graph/
    # edge_id stripped first) rather than x itself: `[.data.frame` calls
    # length(x) internally, which would otherwise dispatch to
    # length.edge_vec() (the *edge count*, not the column count) and corrupt
    # the slice -- most visibly, but not only, when there are 0 attribute
    # columns.
    nodes <- attr(x, "nodes")
    directed <- attr(x, "directed")
    edge_id <- attr(x, "edge_id")
    body <- x
    attr(body, "nodes") <- NULL
    attr(body, "directed") <- NULL
    attr(body, "graph") <- NULL
    attr(body, "edge_id") <- NULL
    class(body) <- "data.frame"
    new_attrs <- body[idx, , drop = FALSE]
    return(new_edge_vec_backend(
      new_attrs,
      nodes = nodes,
      directed = directed,
      graph = graph,
      edge_id = edge_id[idx]
    ))
  }

  # Hyperedge (pure-R) case.
  fields <- edge_vec_fields_df(x)[idx, , drop = FALSE]
  # An NA index gives a hyperedge role NULL, an empty node set; make it NA,
  # the missing position an ordinary role gets (its length-1 case).
  for (role in c("from", "to")) {
    if (is.list(fields[[role]])) fields[[role]][is.na(idx)] <- list(NA_integer_)
  }
  new_edge_vec_fields(
    fields = fields,
    nodes = attr(x, "nodes"),
    directed = attr(x, "directed")
  )
}

#' @export
`[[.edge_vec` <- function(x, i, ...) {
  check_scalar_index(i)
  x[i]
}

# Assigning edges from another edge_vec is a disjoint union, like c() and
# vctrs::vec_assign(): `value`'s nodes are appended to `x`'s and its edges
# keep pointing at them, even if both have the same nodes.
#' @export
`[<-.edge_vec` <- function(x, i, value) {
  if (!inherits(value, "edge_vec")) {
    stop("Can only assign `edge_vec` objects into an `edge_vec`.", call. = FALSE)
  }
  pos <- seq_along(x)
  if (missing(i)) {
    pos[] <- length(x) + seq_along(value)
  } else {
    pos[i] <- length(x) + seq_along(value)
  }
  c(x, value)[pos]
}

#' @export
`[[<-.edge_vec` <- function(x, i, value) {
  check_scalar_index(i)
  x[i] <- value
  x
}

#' @export
as.list.edge_vec <- function(x, ...) {
  lapply(seq_along(x), function(i) x[i])
}

#' @export
length.edge_vec <- function(x) {
  graph <- attr(x, "graph")
  if (!is.null(graph)) {
    length(attr(x, "edge_id"))
  } else {
    length(edge_vec_data(x)[["from"]]) # `from` is aligned 1:1 with edges
  }
}

#' @export
c.edge_vec <- function(...) {
  xs <- Filter(Negate(is.null), list(...))
  if (!all(vapply(xs, inherits, logical(1), what = "edge_vec"))) {
    stop("Can only combine `edge_vec` objects with other `edge_vec` objects.", call. = FALSE)
  }

  directed <- attr(xs[[1]], "directed")
  if (!all(vapply(xs, function(x) identical(attr(x, "directed"), directed), logical(1)))) {
    stop("Can't combine `edge_vec` objects with different `directed`.", call. = FALSE)
  }

  # Disjoint union: concatenate the node vectors, then offset each source's
  # from/to positions by the number of nodes already placed ahead of it.
  node_sizes <- vapply(xs, function(x) NROW(attr(x, "nodes")), integer(1))
  offsets <- cumsum(c(0L, utils::head(node_sizes, -1L)))

  fields <- Map(function(x, offset) {
    f <- edge_vec_fields_df(x)
    f[["from"]] <- offset_incidence(f[["from"]], offset)
    f[["to"]] <- offset_incidence(f[["to"]], offset)
    f
  }, xs, offsets)

  # Up-cast to a hyperedge column if any source uses one for this role, so an
  # ordinary and a hyperedge edge_vec can still be combined.
  for (col in c("from", "to")) {
    if (any(vapply(fields, function(f) is.list(f[[col]]), logical(1)))) {
      fields <- lapply(fields, function(f) {
        f[[col]] <- as_incidence_list(f[[col]])
        f
      })
    }
  }

  # A fresh disjoint-union graph is built from the concatenated (offset)
  # from/to -- simplest option (_dev/RUST_BACKEND.md §2.4); no benchmarks
  # justify a dedicated Rust union primitive yet.
  new_edge_vec_fields(
    fields = as.list(rbind_fill(fields)),
    nodes = combine_values(lapply(xs, function(x) attr(x, "nodes"))),
    directed = directed
  )
}

#' @rdname reorient
#' @export
edges.edge_vec <- function(x, ...) {
  x
}

#' @rdname reorient
#' @export
nodes.edge_vec <- function(x, ...) {
  graph <- attr(x, "graph")
  if (!is.null(graph) && identical(attr(x, "edge_id"), seq_len(graph$n_edges()))) {
    # Free reorientation (_dev/RUST_BACKEND.md §2.3): x still covers every
    # edge of `graph`, in `graph`'s own order (nothing sliced away), so its
    # attribute table is already exactly what a node_vec's `edges` attribute
    # would be -- same graph pointer, no Rust call, no from/to materialised.
    return(new_node_vec_backend(
      attr(x, "nodes"),
      graph = graph,
      edges = edge_vec_data(x),
      directed = attr(x, "directed")
    ))
  }

  # General path: a hyperedge edge_vec, or one sliced away from the full
  # edge set its graph represents -- from/to plus any edge attribute columns
  # so attributes reorient with the topology; new_node_vec() builds a fresh
  # graph from exactly the edges present.
  edge_table <- tibble::as_tibble(edge_vec_fields_df(x))

  new_node_vec(
    x = attr(x, "nodes"),
    edges = edge_table,
    directed = attr(x, "directed")
  )
}

# Registered dynamically for pillar via zzz.R; abbreviated type header, e.g. "E[chr]".
type_sum.edge_vec <- function(x, ...) {
  nodes <- attr(x, "nodes")
  # Drop pillar's own "[,ncol]" suffix for a data-frame `nodes` (e.g. "df[,1]"),
  # which would double up as "E[df[,1]]"; keep just "E[df]".
  abbr <- if (is.data.frame(nodes)) "df" else pillar::type_sum(nodes, ...)
  paste0("E[", abbr, "]")
}

# Registered dynamically for ggplot2 via zzz.R. There's no scale for an
# edge_vec, so give a helpful error rather than ggplot2's default.
scale_type.edge_vec <- function(x) {
  stop(
    "Cannot add an edge vector to a plot, use format() to plot with your edges.",
    call. = FALSE
  )
}

#' @importFrom utils .DollarNames
#' @export
rep.edge_vec <- function(x, ...) {
  x[rep(seq_along(x), ...)]
}

# Edges are unnamed. The underlying list's names are its fields, which
# tibble/vctrs would otherwise erase with `names(x) <- NULL`.
#' @export
names.edge_vec <- function(x) {
  NULL
}

#' @export
`names<-.edge_vec` <- function(x, value) {
  if (!is.null(value)) {
    stop("`edge_vec` objects can't have names.", call. = FALSE)
  }
  x
}

#' @export
.DollarNames.edge_vec <- function(x, pattern){
  # from/to live in the graph backend, not the body, unless x is a hyperedge.
  utils::findMatches(pattern, unique(c("from", "to", names(edge_vec_data(x)))))
}

#' @export
`$.edge_vec` <- function(x, name){
  name <- as.character(name)
  fields <- edge_vec_data(x)

  if (name %in% c("from", "to")) {
    return(incidence_slice(attr(x, "nodes"), edge_vec_endpoints(x)[[name]]))
  }

  if (name %in% names(fields)) {
    return(fields[[name]])
  }

  stop(
    sprintf("`$.edge_vec` only supports `from`, `to`, or an edge attribute, not `%s`.", name),
    call. = FALSE
  )
}

# Edges are equal when they join the same node values in each role and have
# the same attributes, regardless of their nodes' positions, so equality
# holds across edge_vecs (and through c(), which offsets positions).
#' @export
duplicated.edge_vec <- function(x, incomparables = FALSE, ...) {
  fields <- edge_vec_value_fields(x)
  # Base duplicated() can't compare a list (hyperedge) column, so key each
  # node set by its deparsed values instead.
  fields <- lapply(fields, function(col) {
    if (is.list(col) && !is.data.frame(col)) vapply(col, function(v) paste(deparse(v), collapse = ""), character(1)) else col
  })
  key <- do.call(cbind, lapply(fields, function(col) {
    if (is.data.frame(col)) as.data.frame(col) else data.frame(col, stringsAsFactors = FALSE)
  }))
  if (length(key) == 0L) {
    return(logical(length(x)))
  }
  duplicated(key, incomparables = incomparables, ...)
}

#' @export
unique.edge_vec <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x, incomparables = incomparables, ...)]
}

# The per-edge values that identify an edge: the node values in each role
# (one node-value slice per edge for a hyperedge role), then the edge
# attributes. Positions stand in for node values when `nodes` holds none.
# A missing hyperedge role (an NA position) becomes NULL, the missing value
# of a list.
edge_vec_value_fields <- function(x) {
  nodes <- attr(x, "nodes")
  fields <- as.list(edge_vec_fields_df(x))
  for (role in c("from", "to")) {
    pos <- fields[[role]]
    if (!is.list(pos)) {
      if (has_node_values(nodes)) fields[[role]] <- unname_rows(slice_rows(nodes, pos))
      next
    }
    fields[[role]] <- lapply(pos, function(idx) {
      if (length(idx) == 1L && is.na(idx)) {
        NULL
      } else if (has_node_values(nodes)) {
        unname_rows(slice_rows(nodes, idx))
      } else {
        as.integer(idx)
      }
    })
  }
  fields
}

# One logical per edge: TRUE when every field is missing, as vctrs detects
# from vec_proxy_equal() (a data-frame field, such as data-frame node values,
# only when all its columns are).
#' @export
is.na.edge_vec <- function(x) {
  missing <- lapply(edge_vec_value_fields(x), field_is_missing)
  Reduce(`&`, missing, rep(TRUE, length(x)))
}

#' @export
anyNA.edge_vec <- function(x, recursive = FALSE) {
  any(is.na(x))
}

field_is_missing <- function(col) {
  if (is.data.frame(col)) {
    return(Reduce(`&`, lapply(col, field_is_missing), rep(TRUE, nrow(col))))
  }
  if (is.list(col)) {
    return(vapply(col, is.null, logical(1)))
  }
  is.na(col)
}

# Whether `nodes` holds values to identify nodes by, rather than being the
# default empty data frame.
has_node_values <- function(nodes) {
  !(is.data.frame(nodes) && length(nodes) == 0L)
}

unname_rows <- function(x) {
  if (is.data.frame(x)) x else unname(x)
}

#' @export
as_tibble.edge_vec <- function(x, ...) {
  tibble::as_tibble(edge_vec_fields_df(x))
}

#' @export
as.data.frame.edge_vec <- function(x, ...) {
  edge_vec_fields_df(x)
}
