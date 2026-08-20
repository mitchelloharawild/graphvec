#' Graph vector along nodes
#'
#' A `node_vec` is a vector of graph nodes with associated edges stored as
#' attributes.
#'
#' @param x A vector representing the nodes in the graph.
#' @param from Integer vector of 'from' node positions into `x`. Hyperedges
#' (multiple 'from' nodes per edge) are not yet supported.
#' @param to Integer vector of 'to' node positions into `x`.
#' @param ... Named edge attribute vectors (e.g. `weight = c(1, 2, 5)`),
#' recycled to the number of edges. `from` and `to` are reserved and cannot
#' be used as attribute names. Attribute columns are stored on the edge
#' table itself, so they slice, replicate, and reorient with the edges they
#' belong to (see [nodes()]/[edges()]).
#' @param directed A single logical value: is incidence ordered (`from` -> `to`)
#' or symmetric?
#'
#' @return A `node_vec` object.
#'
#' @examples
#'
#' g <- node_vec(
#'  x = factor(c("A", "B", "C")),
#'  from = c(1L, 2L, 1L),
#'  to = c(2L, 3L, 1L),
#'  weight = c(1, 2, 5)
#' )
#' g
#'
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   igraph::as.igraph(g)
#' }
#'
#' @export
node_vec <- function(x = list(), from = integer(), to = integer(), ..., directed = TRUE) {
  stopifnot(is.atomic(x) || is.list(x))
  stopifnot(is.logical(directed), length(directed) == 1)
  stopifnot(!is.na(directed))

  fields <- new_edge_attrs(from, to, ...)
  stopifnot(is.integer(fields$from))
  stopifnot(is.integer(fields$to))
  edges <- tibble::as_tibble(fields)

  new_node_vec(x = x, edges = edges, directed = directed)
}

#' Constructor function for node_vec
#'
#' @param x A vector representing the nodes in the graph.
#' @param edges A data frame with columns `from` and `to` representing the edges
#' @param directed A single logical value: is incidence ordered (`from` -> `to`)
#' or symmetric?
#' @return A `node_vec` object.
#'
#' @examples
#' new_node_vec(
#'   x = c("A", "B", "C"),
#'   edges = data.frame(from = c(1L, 2L), to = c(2L, 3L))
#' )
#'
#' @export
new_node_vec <- function(x = list(), edges = data.frame(from = integer(), to = integer()), directed = TRUE) {
  # "data.frame" is excluded from the external class so no data.frame generic
  # can hijack a data-frame-backed x; the true class is cached below so
  # strip_node_vec() can restore it when real data-frame semantics are needed.
  value_class <- class(x)
  structure(
    x,
    class = c("node_vec", setdiff(value_class, "data.frame")),
    value_class = value_class,
    edges = edges,
    directed = directed
  )
}

#' @export
format.node_vec <- function(x, ...){
  node_label(node_vec_data(x), ...)
}

#' @export
as.character.node_vec <- function(x, ...) {
  as.character(node_vec_data(x), ...)
}

#' @export
print.node_vec <- function(x, ...) {
  cat(sprintf("<node_vec[%d]>\n", length(x)))
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

# Registered dynamically for pillar via zzz.R.
pillar_shaft.node_vec <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format(x, ...), align = "left", min_width = 10)
}

# Drops "node_vec" from x's class and clears the edges/directed attributes,
# leaving x exactly as it was passed to new_node_vec().
strip_node_vec <- function(x) {
  # Restored from the cached "value_class" attribute rather than x's current
  # (node_vec-layered) class, which has "data.frame" excluded.
  value_class <- attr(x, "value_class")
  attr(x, "edges") <- NULL
  attr(x, "directed") <- NULL
  attr(x, "value_class") <- NULL
  oldClass(x) <- NULL
  if (!identical(value_class, class(x))) oldClass(x) <- value_class
  x
}

# The underlying value x was constructed from.
node_vec_data <- strip_node_vec

# A per-element label for a vector of node values: paste columns together
# for a data frame of node attributes, or format the values directly.
node_label <- function(x, ...) {
  if (is.data.frame(x)) {
    do.call(paste, c(x, sep = ":"))
  } else {
    format(x, ...)
  }
}

# Induced-subgraph edge remap for a node_vec sliced from `n` nodes down to
# `idx` (the new node's old position, with repeats for replicated nodes and
# NA for positions with no source). Edges losing an endpoint are dropped;
# edges whose endpoints were replicated are cloned once per combination of
# replica positions, carrying the same attribute values as the original.
node_vec_reindex_edges <- function(n, idx, edges) {
  new_positions <- vector("list", n)
  for (j in seq_along(idx)) {
    p <- idx[j]
    if (is.na(p)) next
    new_positions[[p]] <- c(new_positions[[p]], j)
  }

  from <- edges[["from"]]
  to <- edges[["to"]]

  new_from <- integer()
  new_to <- integer()
  new_source <- integer()

  for (e in seq_along(to)) {
    from_opts <- new_positions[[from[e]]]
    to_opts <- new_positions[[to[e]]]
    if (length(from_opts) == 0L || length(to_opts) == 0L) next

    combos <- expand.grid(from = from_opts, to = to_opts, KEEP.OUT.ATTRS = FALSE)
    new_from <- c(new_from, combos$from)
    new_to <- c(new_to, combos$to)
    new_source <- c(new_source, rep(e, nrow(combos)))
  }

  new_edges <- edges[new_source, , drop = FALSE]
  rownames(new_edges) <- NULL
  new_edges[["from"]] <- new_from
  new_edges[["to"]] <- new_to
  new_edges
}

#' Subset a node_vec
#'
#' Slicing a `node_vec` is an induced subgraph: edges that lose an endpoint
#' are dropped, surviving edges are remapped to the new positions, and
#' replicated nodes (e.g. `x[c(1, 1, 2)]`) clone the edges incident to the
#' original.
#'
#' @param x A `node_vec`.
#' @param i Indices to select, as for `` `[` ``.
#' @param ... Passed on.
#' @return A `node_vec` containing only the selected nodes, with `edges`
#'   restricted to the induced subgraph.
#' @examples
#' g <- node_vec(
#'   x = c("A", "B", "C"),
#'   from = c(1L, 2L),
#'   to = c(2L, 3L)
#' )
#' g[1:2]
#' @keywords internal
#' @export
`[.node_vec` <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }

  n <- length(x)
  idx <- seq_len(n)[i]

  # slice_rows(), not base `[`: a bare index on a data-frame-valued x
  # otherwise means "select columns", not "select rows".
  val <- slice_rows(strip_node_vec(x), idx)
  new_node_vec(
    x = val,
    edges = node_vec_reindex_edges(n, idx, attr(x, "edges")),
    directed = attr(x, "directed")
  )
}

#' @export
length.node_vec <- function(x) {
  NROW(strip_node_vec(x))
}

# Registered dynamically for pillar via zzz.R; abbreviated type header, e.g. "N[chr]".
type_sum.node_vec <- function(x, ...) {
  paste0("N[", pillar::type_sum(node_vec_data(x), ...), "]")
}

#' @rdname reorient
#' @export
nodes.node_vec <- function(x, ...) {
  x
}

#' @rdname reorient
#' @export
edges.node_vec <- function(x, ...) {
  edge_table <- attr(x, "edges")

  # Attribute columns beyond from/to travel across reorientation too.
  attr_names <- setdiff(names(edge_table), c("from", "to"))
  do.call(new_edge_vec, c(
    list(
      from = edge_table[["from"]],
      to = edge_table[["to"]]
    ),
    as.list(edge_table[attr_names]),
    list(nodes = node_vec_data(x), directed = attr(x, "directed"))
  ))
}

#' @export
unique.node_vec <- function(x, incomparables = FALSE, ...) {
  # Value-based: drops duplicate-valued nodes by first occurrence, via
  # [.node_vec's induced-subgraph rules, so edges incident to a dropped
  # duplicate are dropped rather than redirected onto the kept node.
  x[!duplicated(node_vec_data(x), incomparables = incomparables, ...)]
}

#' @export
c.node_vec <- function(...) {
  xs <- Filter(Negate(is.null), list(...))
  if (!all(vapply(xs, inherits, logical(1), what = "node_vec"))) {
    stop("Can only combine `node_vec` objects with other `node_vec` objects.", call. = FALSE)
  }

  directed <- attr(xs[[1]], "directed")
  if (!all(vapply(xs, function(x) identical(attr(x, "directed"), directed), logical(1)))) {
    stop("Can't combine `node_vec` objects with different `directed`.", call. = FALSE)
  }

  # Disjoint union: concatenate node values, then offset each graph's edge
  # positions by the number of nodes already placed ahead of it.
  sizes <- vapply(xs, length, integer(1))
  offsets <- cumsum(c(0L, utils::head(sizes, -1L)))

  edges <- Map(function(x, offset) {
    e <- attr(x, "edges")
    e[["from"]] <- e[["from"]] + offset
    e[["to"]] <- e[["to"]] + offset
    e
  }, xs, offsets)

  new_node_vec(
    x = combine_values(lapply(xs, node_vec_data)),
    edges = rbind_fill(edges),
    directed = directed
  )
}

#' @export
rep.node_vec <- function(x, ...) {
  # Delegates to [.node_vec, which already clones a replicated node's incident edges.
  x[rep(seq_along(x), ...)]
}
