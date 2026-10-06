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

# Drops "edge_vec" from x's class and clears the nodes/directed attributes,
# leaving the plain from/to/attrs fields as an unclassed named list.
strip_edge_vec <- function(x) {
  attr(x, "nodes") <- NULL
  attr(x, "directed") <- NULL
  cls <- setdiff(oldClass(x), "edge_vec")
  oldClass(x) <- NULL
  if (!identical(cls, class(x))) oldClass(x) <- cls
  x
}

edge_vec_data <- strip_edge_vec

# The fields, rewrapped as a genuine data frame for call sites that need
# real row-wise semantics (`[.data.frame`/rbind() require it).
edge_vec_fields_df <- function(x) {
  edge_fields_df(edge_vec_data(x))
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
# table; used internally by [.edge_vec/c.edge_vec to skip re-recycling.
# class is set to exactly "edge_vec" so no data.frame generic can hijack it.
new_edge_vec_fields <- function(fields, nodes = data.frame(), directed = TRUE) {
  attr(fields, "row.names") <- NULL # stray leftover once no longer classed data.frame
  structure(fields, class = "edge_vec", nodes = nodes, directed = directed)
}

#' @export
format.edge_vec <- function(x, ...){
  key_data <- attr(x, "nodes")
  fields <- edge_vec_data(x)
  # -- undirected
  # -> directed
  arrow <- if (isTRUE(attr(x, "directed"))) "->" else "--"
  sprintf(
    "[%s]%s[%s]",
    incidence_label(key_data, fields[["from"]]),
    arrow,
    incidence_label(key_data, fields[["to"]])
  )
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
  new_edge_vec_fields(
    fields = edge_vec_fields_df(x)[idx, , drop = FALSE],
    nodes = attr(x, "nodes"),
    directed = attr(x, "directed")
  )
}

#' @export
length.edge_vec <- function(x) {
  length(edge_vec_data(x)[["from"]]) # `from` is aligned 1:1 with edges
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

  new_edge_vec_fields(
    fields = rbind_fill(fields),
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
  # from/to plus any edge attribute columns, so attributes reorient with the topology.
  edge_table <- tibble::as_tibble(edge_vec_data(x))

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
  utils::.DollarNames(edge_vec_data(x), pattern)
}

#' @export
`$.edge_vec` <- function(x, name){
  name <- as.character(name)
  fields <- edge_vec_data(x)

  if (name %in% c("from", "to")) {
    return(incidence_slice(attr(x, "nodes"), fields[[name]]))
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
edge_vec_value_fields <- function(x) {
  nodes <- attr(x, "nodes")
  fields <- as.list(edge_vec_data(x))
  for (role in c("from", "to")) {
    pos <- fields[[role]]
    if (!has_node_values(nodes)) {
      if (is.list(pos)) fields[[role]] <- lapply(pos, as.integer)
      next
    }
    fields[[role]] <- if (is.list(pos)) {
      lapply(pos, function(idx) unname_rows(slice_rows(nodes, idx)))
    } else {
      unname_rows(slice_rows(nodes, pos))
    }
  }
  fields
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
  tibble::as_tibble(edge_vec_data(x))
}

#' @export
as.data.frame.edge_vec <- function(x, ...) {
  as.data.frame(edge_vec_data(x))
}
