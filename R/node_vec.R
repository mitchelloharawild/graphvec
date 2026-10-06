#' Graph vector along nodes
#'
#' A `node_vec` is a vector of graph nodes with associated edges stored as
#' attributes.
#'
#' @param x A vector representing the nodes in the graph.
#' @param from Integer vector of 'from' node positions into `x`, one per
#' edge. A list of integer vectors instead opts into hyperedges: each
#' element gives the (zero or more) 'from' positions for that edge, an
#' ordinary edge being the length-1 case.
#' @param to Integer vector of 'to' node positions into `x`, or a list of
#' integer vectors for hyperedges, the same way as `from`.
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
  stopifnot(is_valid_incidence(fields$from))
  stopifnot(is_valid_incidence(fields$to))
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
  from <- edges[["from"]]
  to <- edges[["to"]]

  # Hyperedges (list-valued from/to) are out of scope for the Rust backend
  # (_dev/RUST_BACKEND.md) -- keep today's edges-attribute representation
  # exactly as-is, unaccelerated.
  if (is.list(from) || is.list(to)) {
    # "data.frame" is excluded from the external class so no data.frame
    # generic can hijack a data-frame-backed x; the true class is cached
    # below so strip_node_vec() can restore it when real data-frame
    # semantics are needed.
    value_class <- class(x)
    return(structure(
      x,
      class = c("node_vec", setdiff(value_class, "data.frame")),
      value_class = value_class,
      edges = edges,
      directed = directed
    ))
  }

  # Ordinary case: topology moves into a shared GraphBackend; `edges` keeps
  # only the attribute columns (everything but from/to), aligned 1:1 with
  # the graph's edge order -- this is what removes the two-copies problem
  # `_dev/RUST_BACKEND.md` describes.
  graph <- graphvec_backend_new(NROW(x), from, to, directed)
  attrs <- edges[setdiff(names(edges), c("from", "to"))]
  new_node_vec_backend(x, graph = graph, edges = attrs, directed = directed)
}

# Low-level constructor for the non-hyperedge (Rust-backed) case. `graph` is
# the shared GraphBackend the nodes belong to, `edges` the attribute-only
# table aligned with `graph`'s edge order, and `node_id` the position in
# `graph` of each element of `x` (one per element, NA for a missing node
# with no position). A node_vec built from scratch covers its whole graph
# (`node_id` is `seq_len(NROW(x))`); a slice keeps its parent's `graph` and
# `edges` and only subsets `x` and `node_id`, the same way an edge_vec slice
# keeps `graph` and subsets `edge_id` (see `[.node_vec`). Used by
# new_node_vec() and by nodes.edge_vec()'s free (no-Rust-call)
# reorientation.
new_node_vec_backend <- function(x, graph, edges, directed, node_id = seq_len(NROW(x))) {
  value_class <- class(x)
  structure(
    x,
    class = c("node_vec", setdiff(value_class, "data.frame")),
    value_class = value_class,
    graph = graph,
    edges = edges,
    directed = directed,
    node_id = node_id
  )
}

# Whether a (Rust-backed) node_vec is exactly its graph's node set, in the
# graph's own order: then `graph` and `edges` already describe its edges,
# with no induced view to compute.
node_vec_is_full <- function(x) {
  identical(attr(x, "node_id"), seq_len(attr(x, "graph")$n_nodes()))
}

# The node_vec's own graph: the subgraph `graph` induces on `node_id`,
# renumbered to positions 1..length(x), as a node_vec covering its whole
# (new) graph. Everything that reads a node_vec's edges (edges(),
# as.igraph(), the topology operations, waldo, c()) goes through this, so a
# slice sees only the edges among its own nodes: an edge is dropped once
# either end is no longer selected, and a replicated node (`x[c(1, 1)]`)
# clones its edges once per replica, as GraphBackend$induced_subgraph()
# defines.
#
# Computed lazily, on read, rather than when slicing (_dev/RUST_BACKEND.md
# §2.2's "selection backing"): slicing is the hot path (dplyr verbs and
# vctrs restores slice constantly) and costs O(length(i)) this way, while
# the O(N + M) induced view is only paid for when edges are actually read.
# A node_vec that is already full is returned as-is, for free. Nothing is
# cached: the objects are immutable values, and an environment attribute to
# cache into would break identical() and serialisation.
node_vec_compact <- function(x) {
  graph <- attr(x, "graph")
  if (is.null(graph) || node_vec_is_full(x)) {
    return(x)
  }
  remap <- graphvec_backend_induced_subgraph(graph, attr(x, "node_id"))
  attrs <- attr(x, "edges")[remap$source_edge, , drop = FALSE]
  rownames(attrs) <- NULL
  directed <- attr(x, "directed")
  new_node_vec_backend(
    node_vec_data(x),
    graph = graphvec_backend_new(length(x), remap$from, remap$to, directed),
    edges = attrs,
    directed = directed
  )
}

# `x`'s graph with its node values swapped for `values` (one per node), for
# operations that relabel nodes without touching the graph: assigning plain
# values with `[<-`, and vctrs casts. Keeps the graph identity.
node_vec_with_values <- function(x, values) {
  graph <- attr(x, "graph")
  if (is.null(graph)) {
    return(new_node_vec(values, edges = attr(x, "edges"), directed = attr(x, "directed")))
  }
  new_node_vec_backend(
    values,
    graph = graph,
    edges = attr(x, "edges"),
    directed = attr(x, "directed"),
    node_id = attr(x, "node_id")
  )
}

#' @export
format.node_vec <- function(x, ...){
  node_label(node_vec_data(x), ...)
}

# One string per node: the values themselves, or a data-frame-backed
# node_vec's formatted label (as.character() of the data frame itself would
# give one string per column).
#' @export
as.character.node_vec <- function(x, ...) {
  if (is_df_node_vec(x)) {
    return(trimws(format(x, ...)))
  }
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
  attr(x, "graph") <- NULL # absent (NULL already) for the hyperedge case
  attr(x, "node_id") <- NULL # likewise
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
  if (is.data.frame(x) && ncol(x) == 0L) {
    # No node data: label nodes by position.
    as.character(seq_len(nrow(x)))
  } else if (is.data.frame(x)) {
    do.call(paste, c(x, sep = ":"))
  } else {
    format(x, ...)
  }
}

# Induced-subgraph edge remap for a node_vec sliced from `n` nodes down to
# `idx` (the new node's old position, with repeats for replicated nodes and
# NA for positions with no source). An edge is dropped if any node it
# references (in either role, and for every member of a hyperedge role) was
# dropped; an edge whose referenced nodes were replicated is cloned once per
# combination of replica positions, carrying the same attribute values as
# the original. `from`/`to` stay whatever shape (plain or hyperedge) they
# arrived in.
#
# Hyperedge-only fallback: the ordinary (non-hyperedge) case is handled by
# GraphBackend$induced_subgraph() instead (_dev/RUST_BACKEND.md), which is
# the same computation done in Rust over a plain (non-list) from/to.
node_vec_reindex_edges <- function(n, idx, edges) {
  new_positions <- vector("list", n)
  for (j in seq_along(idx)) {
    p <- idx[j]
    if (is.na(p)) next
    new_positions[[p]] <- c(new_positions[[p]], j)
  }

  from <- edges[["from"]]
  to <- edges[["to"]]
  from_is_hyper <- is.list(from)
  to_is_hyper <- is.list(to)

  new_from <- list()
  new_to <- list()
  new_source <- integer()

  for (e in seq_along(to)) {
    from_val <- if (from_is_hyper) from[[e]] else from[e]
    to_val <- if (to_is_hyper) to[[e]] else to[e]

    from_opts <- incidence_options(from_val, new_positions)
    to_opts <- incidence_options(to_val, new_positions)
    if (length(from_opts) == 0L || length(to_opts) == 0L) next

    # `from` varies fastest, `to` slowest -- the same order expand.grid()
    # would produce for expand.grid(from = from_opts, to = to_opts).
    for (t in to_opts) {
      for (f in from_opts) {
        new_from[[length(new_from) + 1L]] <- f
        new_to[[length(new_to) + 1L]] <- t
        new_source <- c(new_source, e)
      }
    }
  }

  new_edges <- edges[new_source, , drop = FALSE]
  rownames(new_edges) <- NULL
  new_edges[["from"]] <- if (from_is_hyper) as_incidence_list(new_from) else as.integer(unlist(new_from, use.names = FALSE))
  new_edges[["to"]] <- if (to_is_hyper) as_incidence_list(new_to) else as.integer(unlist(new_to, use.names = FALSE))
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

  graph <- attr(x, "graph")
  if (!is.null(graph)) {
    # Ordinary case: keep the whole graph and select positions in it, like
    # an edge_vec slice. The induced subgraph is only worked out when the
    # edges are read (node_vec_compact()).
    new_node_vec_backend(
      val,
      graph = graph,
      edges = attr(x, "edges"),
      directed = attr(x, "directed"),
      node_id = attr(x, "node_id")[idx]
    )
  } else {
    new_node_vec(
      x = val,
      edges = node_vec_reindex_edges(n, idx, attr(x, "edges")),
      directed = attr(x, "directed")
    )
  }
}

# A plain `value` relabels the selected nodes, keeping the graph and every
# edge. A
# node_vec `value` brings its own graph: like c() and vctrs::vec_assign(),
# the result is the disjoint union with the replaced nodes swapped out, so
# `x`'s edges to them are dropped and `value`'s edges among the assigned
# nodes are kept.
#' @export
`[<-.node_vec` <- function(x, i, value) {
  if (missing(i)) {
    i <- seq_along(x)
  }
  if (inherits(value, "node_vec")) {
    pos <- seq_along(x)
    pos[i] <- length(x) + seq_along(value)
    return(c(x, value)[pos])
  }
  data <- node_vec_data(x)
  if (is.data.frame(data)) {
    data[i, ] <- value
    rownames(data) <- NULL
  } else {
    data[i] <- value
  }
  if (NROW(data) != length(x)) {
    # Assigning past the end grows `x` with new, unconnected nodes.
    x <- x[seq_len(NROW(data))]
  }
  node_vec_with_values(x, data)
}

#' @export
length.node_vec <- function(x) {
  NROW(strip_node_vec(x))
}

# A data-frame-backed node_vec's underlying list names are its columns, not
# names for its nodes, so it has none. Otherwise vctrs/tibble would read the
# columns as element names, and erase the columns with `names(x) <- NULL`.
#' @export
names.node_vec <- function(x) {
  if (is_df_node_vec(x)) NULL else NextMethod()
}

#' @export
`names<-.node_vec` <- function(x, value) {
  if (!is_df_node_vec(x)) {
    return(NextMethod())
  }
  if (!is.null(value)) {
    stop("A `node_vec` of data frame values can't have names.", call. = FALSE)
  }
  x
}

is_df_node_vec <- function(x) {
  "data.frame" %in% attr(x, "value_class")
}

# A single node, as a length-1 node_vec (like `[[` on agg_vec, edge_vec and
# base S3 vectors such as Date and factor), never its bare value or, for a
# data-frame-backed node_vec, one of its columns.
#' @export
`[[.node_vec` <- function(x, i, ...) {
  x[element_position(x, i)]
}

# The same as `[<-` on a single position. `value` is one node: a length-1
# node_vec, a single plain value, or a 1-row data frame.
#' @export
`[[<-.node_vec` <- function(x, i, value) {
  check_scalar_index(i)
  if (NROW(value) != 1L) {
    stop("`value` must be a single node.", call. = FALSE)
  }
  x[i] <- value
  x
}

#' @export
as.list.node_vec <- function(x, ...) {
  out <- lapply(seq_along(x), function(i) x[i])
  names(out) <- names(x)
  out
}

# A single column, as for as.data.frame.agg_vec(), whatever the node values.
#' @export
as.data.frame.node_vec <- function(x, row.names = NULL, optional = FALSE, ...,
    nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")) {
  force(nm)
  as.data.frame.vector(x, row.names = row.names, optional = optional, ..., nm = nm)
}

# Value-based and row-wise for data frame values, the same duplicates that
# unique.node_vec() drops.
#' @export
duplicated.node_vec <- function(x, incomparables = FALSE, ...) {
  duplicated(node_vec_data(x), incomparables = incomparables, ...)
}

# Ranks for order()/sort()/dplyr::desc() by node value; data frame values
# sort row-wise, column by column, as vctrs::vec_order() sorts them.
#' @export
xtfrm.node_vec <- function(x) {
  data <- node_vec_data(x)
  if (is.data.frame(data)) {
    return(rank_rows(list(data), length(x)))
  }
  xtfrm(data)
}

#' @export
anyDuplicated.node_vec <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(node_vec_data(x), incomparables = incomparables, ...)
}

# Registered dynamically for pillar via zzz.R; abbreviated type header, e.g. "N[chr]".
type_sum.node_vec <- function(x, ...) {
  paste0("N[", pillar::type_sum(node_vec_data(x), ...), "]")
}

# Registered dynamically for ggplot2 via zzz.R. Atomic node values are
# plottable, so use their scale; data-frame node values have no scale, so
# give a helpful error rather than ggplot2's default.
scale_type.node_vec <- function(x) {
  x <- node_vec_data(x)
  if (is.data.frame(x)) {
    stop(
      "Cannot add a data-frame-backed node vector to a plot, use format() to plot with your nodes.",
      call. = FALSE
    )
  }
  ggplot2::scale_type(x)
}

#' @rdname reorient
#' @export
nodes.node_vec <- function(x, ...) {
  x
}

#' @rdname reorient
#' @export
edges.node_vec <- function(x, ...) {
  x <- node_vec_compact(x)
  graph <- attr(x, "graph")
  if (!is.null(graph)) {
    # Free reorientation (_dev/RUST_BACKEND.md §2.3) for a node_vec that
    # covers its whole graph: its `edges` attribute is already aligned 1:1
    # with `graph`'s edge order, so this is just a re-wrap -- same graph
    # pointer, same attribute table, no Rust call, no from/to materialised.
    # A slice was first compacted to its own induced subgraph above.
    return(new_edge_vec_backend(
      attrs = attr(x, "edges"),
      nodes = node_vec_data(x),
      directed = attr(x, "directed"),
      graph = graph,
      edge_id = seq_len(graph$n_edges())
    ))
  }

  # Hyperedge path: unchanged.
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

# A full from/to/attrs data frame for a node_vec's edges, whichever way
# they're stored -- the shape both the hyperedge path and c()'s up-casting
# already expect. For the ordinary (Rust-backed) case, from/to are
# materialised transiently from `graph`, never kept as a second persistent
# copy (new_node_vec() strips them back out again once the caller is done).
node_vec_full_edges <- function(x) {
  x <- node_vec_compact(x)
  graph <- attr(x, "graph")
  if (is.null(graph)) {
    return(attr(x, "edges"))
  }
  ends <- graph$edge_endpoints()
  cbind_edge_fields(ends$from, ends$to, attr(x, "edges"))
}

#' @export
unique.node_vec <- function(x, incomparables = FALSE, ...) {
  # Value-based: drops duplicate-valued nodes by first occurrence, via
  # [.node_vec's induced-subgraph rules, so edges incident to a dropped
  # duplicate are dropped rather than redirected onto the kept node.
  x[!duplicated(x, incomparables = incomparables, ...)]
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
    e <- node_vec_full_edges(x)
    e[["from"]] <- offset_incidence(e[["from"]], offset)
    e[["to"]] <- offset_incidence(e[["to"]], offset)
    e
  }, xs, offsets)

  # Up-cast to a hyperedge column if any source uses one for this role, so an
  # ordinary and a hyperedge node_vec can still be combined.
  for (col in c("from", "to")) {
    if (any(vapply(edges, function(e) is.list(e[[col]]), logical(1)))) {
      edges <- lapply(edges, function(e) {
        e[[col]] <- as_incidence_list(e[[col]])
        e
      })
    }
  }

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
