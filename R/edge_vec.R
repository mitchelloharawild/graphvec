#' Graph vector along edges
#'
#' An `edge_vec` is a vector of graph edges with associated node data stored as
#' attributes.
#'
#' @param from Integer vector of 'from' node indices. Hyperedges (multiple
#' 'from' nodes per edge) are not yet supported.
#' @param to Integer vector of 'to' node indices.
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

  stopifnot(is.integer(fields$from))
  stopifnot(is.integer(fields$to))
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
  # data.frame() recycles each field up to the others' length and errors on mismatch.
  as.list(do.call(data.frame, c(list(from = from, to = to), attrs, list(stringsAsFactors = FALSE))))
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
  do.call(data.frame, c(edge_vec_data(x), list(stringsAsFactors = FALSE)))
}

#' Constructor function for edge_vec
#'
#' @param from Integer vector of 'from' node indices.
#' @param to Integer vector of 'to' node indices.
#' @param ... Named edge attribute fields, already recycled to the number of
#' edges.
#' @param nodes Vector of node data (any vector, including a data
#' frame of node attributes).
#' @param directed A single logical value: is incidence ordered (`from` -> `to`)
#' or symmetric?
#' @return An `edge_vec` object.
#'
#' @export
new_edge_vec <- function(from = integer(), to = integer(), ..., nodes = data.frame(), directed = TRUE) {
  fields <- do.call(data.frame, c(list(from = from, to = to), list(...), list(stringsAsFactors = FALSE)))
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
    node_label(slice_rows(key_data, fields[["from"]])),
    arrow,
    node_label(slice_rows(key_data, fields[["to"]]))
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
    f[["from"]] <- f[["from"]] + offset
    f[["to"]] <- f[["to"]] + offset
    f
  }, xs, offsets)

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
.DollarNames.edge_vec <- function(x, pattern){
  utils::.DollarNames(edge_vec_data(x), pattern)
}

#' @export
`$.edge_vec` <- function(x, name){
  name <- as.character(name)
  fields <- edge_vec_data(x)

  if (name %in% c("from", "to")) {
    return(slice_rows(attr(x, "nodes"), fields[[name]]))
  }

  if (name %in% names(fields)) {
    return(fields[[name]])
  }

  stop(
    sprintf("`$.edge_vec` only supports `from`, `to`, or an edge attribute, not `%s`.", name),
    call. = FALSE
  )
}

#' @export
as_tibble.edge_vec <- function(x, ...) {
  tibble::as_tibble(edge_vec_data(x))
}

#' @export
as.data.frame.edge_vec <- function(x, ...) {
  as.data.frame(edge_vec_data(x))
}
