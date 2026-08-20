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
#' igraph::as.igraph(g)
#'
#' @export
node_vec <- function(x = list(), from = integer(), to = integer(), ..., directed = TRUE) {
  # Check inputs
  stopifnot(is.atomic(x) || is.list(x))
  stopifnot(is.logical(directed), length(directed) == 1)
  stopifnot(!is.na(directed))

  # Validate edges
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
#' @export
new_node_vec <- function(x = list(), edges = data.frame(from = integer(), to = integer()), directed = TRUE) {
  # "data.frame" is deliberately excluded from the layered *external* class
  # (unlike every other class x might carry) -- see strip_node_vec()'s
  # comment for why, and _dev/tidy.md §1 for the crash this avoids.
  # x's true, un-truncated class is cached in "value_class" so strip_node_vec()
  # can restore it exactly whenever internal code needs real data-frame (or
  # any other) semantics back.
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

# Not exported directly -- registered dynamically for pillar via
# register_s3_method() in zzz.R (pillar is a Suggests, not an Imports),
# mirroring pillar_shaft.agg_vec() in R/agg_vec.R. Without this,
# UseMethod("pillar_shaft") on a node_vec with no method of its own would
# fall through to whatever x's own class implies -- pillar_shaft.default()
# for most x, but pillar_shaft.data.frame() for a data-frame-backed x (had
# node_vec still exposed "data.frame" in its class), which tries to render x
# as a *nested* tibble and chokes on the edges/directed attributes it isn't
# expecting (see _dev/tidy.md §1). new_node_vec() excluding "data.frame" from
# the external class (see its comment) already prevents that specific
# mis-dispatch; this method is what makes the resulting fallback (formatted
# node labels via format(), not a blank/default rendering) look right.
pillar_shaft.node_vec <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format(x, ...), align = "left", min_width = 10)
}

# Drops "node_vec" from x's class and clears the edges/directed attributes,
# leaving x exactly as it was passed to new_node_vec() -- including no
# explicit class attribute at all, if it had none to begin with (e.g. a
# plain character vector), so that e.g. expect_equal() against the original
# input still holds. Shared by every method below that needs to delegate to
# x's own S3 methods via a legitimate (non-boxed) `x`.
strip_node_vec <- function(x) {
  # Restored from the cached "value_class" attribute, not derived from
  # x's current (node_vec-layered) class -- the latter has "data.frame"
  # excluded (see new_node_vec()), so re-deriving from it would leave a
  # data-frame-shaped x looking like a bare list here, which breaks
  # anything downstream that needs real data-frame semantics back
  # (NROW()/slice_rows() treat a bare list as a vector of columns, not
  # rows -- _dev/tidy.md §1 explains why this is cached rather than
  # re-derived).
  value_class <- attr(x, "value_class")
  attr(x, "edges") <- NULL
  attr(x, "directed") <- NULL
  attr(x, "value_class") <- NULL
  oldClass(x) <- NULL
  if (!identical(value_class, class(x))) oldClass(x) <- value_class
  x
}

# The underlying value x was constructed from -- x's own class and
# attributes are layered underneath node_vec's, so this is just the
# class/attribute-stripped view of x itself, lossless for any vector
# including a data frame of node attributes (its row count, via NROW(),
# is the node count).
node_vec_data <- strip_node_vec

# A per-element label for a vector of node values: paste columns together
# for a data frame of node attributes, or format the values directly for a
# plain vector. Shared by format.node_vec() and format.edge_vec().
node_label <- function(x, ...) {
  if (is.data.frame(x)) {
    do.call(paste, c(x, sep = ":"))
  } else {
    format(x, ...)
  }
}

# Induced-subgraph edge remap for a node_vec sliced from `n` nodes down to
# `idx` (the new node's old position, possibly with repeats for replicated
# nodes and NA for positions with no source). Edges losing an endpoint are
# dropped; edges whose endpoints were replicated are cloned once per
# combination of replica positions (DESIGN.md §3.2, §3.3). Any edge attribute
# columns ride along: a clone of an edge carries the same attribute values as
# its original, since it's the same edge, just incident to a different copy
# of a replicated node.
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
#' @export
`[.node_vec` <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }

  n <- length(x)
  idx <- seq_len(n)[i]

  # slice_rows(), not NextMethod()/base `[`: a bare single index on a
  # data-frame-valued x (e.g. td[2:3]) means "select columns" under base
  # `[.data.frame`, not "select rows" -- slice_rows() already resolves that
  # ambiguity correctly.
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

# Not exported directly -- registered dynamically for pillar via
# register_s3_method() in zzz.R, mirroring pillar_shaft.node_vec() above.
# Gives a tibble column of node_vec an abbreviated type header, e.g.
# "N[chr]", the same shape as vctrs' vec_ptype_abbr() used to produce.
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

  # Any attribute columns beyond from/to travel across reorientation too --
  # they're part of the same edge table edge_vec's own fields are built from.
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
  # Value-based: drops duplicate-valued nodes by first occurrence, same as
  # unique() on any plain vector, via [.node_vec's usual induced-subgraph
  # rules -- edges incident to a dropped duplicate are dropped, not
  # redirected onto the kept node. (Merge-and-redirect-edges is a different,
  # more opinionated "graph-aware" semantics that would need its own,
  # explicitly opted-into function rather than overloading unique().)
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

  # Disjoint union (DESIGN.md §6.4): concatenate node values, then offset
  # each graph's edge positions by the number of nodes already placed ahead
  # of it, so edges keep pointing at the right nodes post-concatenation.
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
  # Delegates to [.node_vec, which already clones a replicated node's
  # incident edges (DESIGN.md §3.2) -- rep() is just a different way of
  # spelling a replicating index.
  x[rep(seq_along(x), ...)]
}
