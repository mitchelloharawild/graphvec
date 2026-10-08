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
#' @section Slicing and combining:
#' Every `node_vec` belongs to a graph. Slicing (`x[i]`, `dplyr::filter()`,
#' `dplyr::arrange()`, ...) keeps that graph and selects nodes in it: the
#' slice's edges are the induced subgraph on its nodes, so an edge is
#' dropped once either of its ends is, and a node repeated within a slice
#' (`x[c(1, 1)]`) is a separate copy of the graph, as for [c()] below.
#'
#' Combining node_vecs, with [c()], `[<-`, `[[<-`, `vctrs::vec_c()`,
#' `dplyr::bind_rows()`, `dplyr::if_else()`, `dplyr::rows_patch()` and the
#' like, groups the nodes by the graph they belong to:
#'
#' * Nodes of the same graph go back into that graph, keeping every edge
#'   between them, including edges between nodes that came from different
#'   inputs: `c(x[1:2], x[3:4])` is `x`, and `if_else(cond, x, x)` or
#'   `x[2] <- x[2]` keep all of `x`'s edges.
#' * The same node coming from two different inputs makes them separate
#'   copies of the graph, a disjoint union: `c(x, x)` has two copies of
#'   every node and edge, and `c(x[1:2], x[2:3])` two copies of `x[2]`, each
#'   with only its own input's edges. Repeats within one slice make copies
#'   the same way: `x[c(1:n, 1:n)]`, `rep(x, 2)` and `vctrs::vec_rep(x, 2)`
#'   are `c(x, x)`, and the k-th repeat of each node (`x[c(1, 1, 2)]`,
#'   `rep(x, each = 2)`) belongs to the k-th copy.
#' * Nodes of different graphs are a disjoint union of those graphs. The
#'   nodes always stay in the order they're combined in.
#'
#' Assigning plain values with `[<-` relabels nodes and keeps the graph.
#' Two nodes are equal when they are (copies of) the same node of the same
#' graph, with the same value. This is what `==`, [duplicated()],
#' [unique()], `vctrs::vec_equal()` and joins use, so `x[1]` equals the
#' first node of `x` (or of `c(x, x)`), but not another node with the same
#' label, nor a node of a separately built node_vec. A node is never equal
#' to a plain value: `x == "A"` is an error, and `match()` and `%in%` (on
#' R >= 4.3) find no match. Compare the values explicitly instead, with
#' `node_values(x) == "A"` or `node_values(x) %in% "A"` (see
#' [node_values()]). Nodes have no order or arithmetic of their own either,
#' so `<`, `+` and the other operators error, as for an `edge_vec`.
#' Hyperedge node_vecs have no graph identity and compare by value.
#'
#' @section Plotting with ggplot2:
#' A `node_vec`'s ggplot2 [scale type][ggplot2::scale_type()] is `"node"`,
#' then the scale types of its values with `"discrete"` in place of
#' `"continuous"` (`"discrete"` for data-frame values), so an extension
#' package can supply default `scale_*_node()` scales. Without one, ggplot2
#' uses its own scales: character, factor and logical nodes get discrete
#' scales, labelled by their values (a factor's in level order), and dates
#' and date-times get date scales. Nodes have no arithmetic, so numeric
#' nodes (and data-frame ones) get discrete scales instead of continuous ones:
#' these work for colour, fill, shape and the like, labelled by [format()],
#' but not for the x and y positions. Plot `node_values(x)` there for a
#' continuous scale, or `format(x)` for a discrete one. Points are grouped
#' by node, so two nodes with the same label are separate groups.
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
  check_dots_options(...names(), "directed")
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
  # `_dev/RUST_BACKEND.md` describes. Every edge joins two of `x`'s nodes:
  # a node_vec has no missing edges (unlike an edge_vec).
  if (anyNA(from) || anyNA(to)) {
    cli::cli_abort("{.arg from} and {.arg to} can't be missing in a {.cls node_vec}.", call = NULL)
  }
  check_edge_positions(from, to, NROW(x))
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
# reorientation. `origin` is each node's identity when it differs from its
# position in `graph` (see node_vec_origin()); NULL otherwise.
new_node_vec_backend <- function(x, graph, edges, directed, node_id = seq_len(NROW(x)), origin = NULL) {
  if (is.integer(node_id) && length(node_id) == graph$n_nodes() && isFALSE(is.unsorted(node_id))) {
    # A selection of the whole graph, in order (see node_vec_is_full()), is
    # kept as R's compact `seq_len()`: the same value, but one whose
    # sortedness R knows, so node_vec_is_full() stays O(1) after `x[1:n]`
    # and the like rather than rescanning it on every read.
    node_id <- seq_len(length(node_id))
  }
  value_class <- class(x)
  structure(
    x,
    class = c("node_vec", setdiff(value_class, "data.frame")),
    value_class = value_class,
    graph = graph,
    edges = edges,
    directed = directed,
    node_id = node_id,
    origin = origin
  )
}

# Each node's identity: the graph it was first a node of (that graph's
# uid()) and its position there, as `list(graph, node_id)` with one entry
# per element (NA for a missing node). Usually just `graph`'s uid() and
# `node_id`; but c() of the same nodes from two inputs (vec_c(x, x), and so
# also vctrs combining join keys) makes disjoint-union copies in a new
# graph, and those copies keep the identity of the nodes they copy in the
# `origin` attribute, so that a node and its copies stay equal. NULL for a
# hyperedge node_vec, which has no graph identity.
node_vec_origin <- function(x) {
  origin <- attr(x, "origin")
  if (!is.null(origin)) {
    return(origin)
  }
  graph <- graph_of(x)
  if (is.null(graph)) {
    return(NULL)
  }
  pos <- attr(x, "node_id")
  uid <- rep(graph$uid(), length(pos))
  uid[is.na(pos)] <- NA
  list(graph = uid, node_id = pos)
}

# `x` with its nodes' identities set to `origin`, which is only kept as an
# attribute when it differs from `x`'s own graph and positions.
node_vec_set_origin <- function(x, origin) {
  attr(x, "origin") <- NULL
  if (!is.null(origin) && !identical(origin, node_vec_origin(x))) {
    attr(x, "origin") <- origin
  }
  x
}

slice_origin <- function(origin, idx) {
  if (is.null(origin)) NULL else lapply(origin, `[`, idx)
}

# The per-node fields that make two nodes equal: the node's identity
# (node_vec_origin()), then its value. So two nodes are equal exactly when
# they are (copies of) the same node of the same graph with the same value,
# whatever their labels say: nodes of different graphs never are, and a node
# relabelled by `[<-` no longer equals the original. A hyperedge node_vec
# has no graph identity and compares by value alone.
node_vec_equal_fields <- function(x) {
  origin <- node_vec_origin(x)
  values <- list(.value = node_vec_data(x))
  if (is.null(origin)) {
    return(values)
  }
  c(list(.graph = origin$graph, .node = origin$node_id), values)
}

# The per-node fields to sort nodes by: their value, then their identity as
# a tie-break, so that only equal nodes tie (the order proxy is also what
# vctrs matches join keys with). Shared by xtfrm() and vctrs' order and
# comparison proxies so they agree.
node_vec_order_fields <- function(x) {
  fields <- node_vec_equal_fields(x)
  fields[c(".value", setdiff(names(fields), ".value"))]
}

# Whether a (Rust-backed) node_vec is exactly its graph's node set, in the
# graph's own order: then `graph` and `edges` already describe its edges,
# with no induced view to compute. `node_id` never repeats a position (`[`
# makes a repeat a separate copy), so as many positions as the graph has
# nodes, in increasing order, are exactly `seq_len(n)` -- no element-wise
# comparison needed. is.unsorted() answers that in O(1) for the compact
# `seq_len()` a whole-graph node_vec holds (R records its sortedness; see
# new_node_vec_backend()), where comparing against `seq_len(n)` would cost
# O(N) on every read.
node_vec_is_full <- function(x) {
  node_id <- attr(x, "node_id")
  length(node_id) == graph_of(x)$n_nodes() && isFALSE(is.unsorted(node_id))
}

# The node_vec's own graph: the subgraph `graph` induces on `node_id`,
# renumbered to positions 1..length(x), as a node_vec covering its whole
# (new) graph. Everything that reads a node_vec's edges (edges(),
# as.igraph(), the topology operations, waldo, c()) goes through this, so a
# slice sees only the edges among its own nodes: an edge is dropped once
# either end is no longer selected, as GraphBackend$induced_subgraph()
# defines. (A slice never repeats a position: `[` makes a repeat a separate
# copy of the graph, see node_vec_assemble().)
#
# Computed lazily, on read, rather than when slicing (_dev/RUST_BACKEND.md
# §2.2's "selection backing"): slicing is the hot path (dplyr verbs and
# vctrs restores slice constantly) and costs O(length(i)) this way, while
# the induced view is only paid for when edges are actually read, and then
# in proportion to the selected nodes' edges for a small slice, not the
# whole graph's. A node_vec that is already full is returned as-is, for
# free. Nothing is cached: the objects are immutable values, and an
# environment attribute to cache into would break identical() and
# serialisation.
node_vec_compact <- function(x) {
  graph <- graph_of(x)
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
  graph <- graph_of(x)
  if (is.null(graph)) {
    return(new_node_vec(values, edges = attr(x, "edges"), directed = attr(x, "directed")))
  }
  new_node_vec_backend(
    values,
    graph = graph,
    edges = attr(x, "edges"),
    directed = attr(x, "directed"),
    node_id = attr(x, "node_id"),
    origin = attr(x, "origin")
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
  attr(x, "origin") <- NULL # only on copies (node_vec_origin())
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
# the original (though `[` no longer passes repeats here: it makes them
# separate copies, see node_vec_assemble()). `from`/`to` stay whatever shape
# (plain or hyperedge) they arrived in.
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
#' Slicing a `node_vec` behaves as an induced subgraph: edges that lose an
#' endpoint are dropped, and surviving edges are remapped to the new
#' positions.
#'
#' Repeating a node (e.g. `x[c(1, 1, 2)]`, [rep()], `vctrs::vec_rep()`)
#' makes a separate copy of the graph for each repeat, as [c()] does: the
#' k-th occurrence of each node belongs to the k-th copy, and each copy has
#' only the edges among its own nodes. So `x[c(1:n, 1:n)]` is `c(x, x)`, and
#' in `x[c(1, 1, 2)]` the edge from `x[1]` to `x[2]` is only kept by the
#' first `x[1]`. Copies still equal the nodes they copy.
#'
#' The slice still remembers which graph its nodes came from (and where in
#' it they are), so putting slices of the same graph back together with
#' [c()], `[<-` or a vctrs/dplyr operation restores the edges between them:
#' `c(x[1:2], x[3:4])` has every edge of `x`, including those joining
#' `x[2]` to `x[3]`. See [node_vec()] for the rule.
#'
#' @param x A `node_vec`.
#' @param i Indices to select, as for `` `[` ``.
#' @param ... Passed on.
#' @return A `node_vec` containing only the selected nodes, whose edges are
#'   the induced subgraph.
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
  idx <- subscript_positions(x, i)

  if (anyDuplicated(idx, incomparables = NA)) {
    # A repeated node is a separate copy of the graph (node_vec_assemble()
    # rule 3), as for rep() and c().
    return(node_vec_assemble(list(x), src = rep(1L, length(idx)), row = idx))
  }

  # slice_rows(), not base `[`: a bare index on a data-frame-valued x
  # otherwise means "select columns", not "select rows".
  val <- slice_rows(strip_node_vec(x), idx)

  graph <- graph_of(x)
  if (!is.null(graph)) {
    # Ordinary case: keep the whole graph and select positions in it, like
    # an edge_vec slice. The induced subgraph is only worked out when the
    # edges are read (node_vec_compact()).
    new_node_vec_backend(
      val,
      graph = graph,
      edges = attr(x, "edges"),
      directed = attr(x, "directed"),
      node_id = attr(x, "node_id")[idx],
      origin = slice_origin(attr(x, "origin"), idx)
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
# edge. A node_vec `value` brings its nodes from its own graph, combined
# with `x`'s remaining nodes by the same rule as c() and vctrs::vec_assign()
# (node_vec_assemble()): nodes of `x`'s own graph go back into it, so
# `x[2] <- x[2]` changes nothing, while nodes of another graph (or nodes
# `x` still holds elsewhere) are a disjoint union, dropping `x`'s edges to
# the replaced nodes.
#' @export
`[<-.node_vec` <- function(x, i, value) {
  if (missing(i)) {
    i <- seq_along(x)
  }
  if (inherits(value, "node_vec")) {
    n <- length(x)
    pos <- stats::setNames(seq_along(x), names(x)) # so a name finds its node
    pos[i] <- n + seq_along(value)
    from_value <- !is.na(pos) & pos > n
    src <- ifelse(from_value, 2L, 1L)
    row <- ifelse(from_value, pos - n, pos)
    return(node_vec_assemble(list(x, value), src, row))
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

# Nodes are equal when they are (copies of) the same node of the same graph
# with the same value (see node_vec_equal_fields()); the same duplicates that
# unique.node_vec() drops.
#' @export
duplicated.node_vec <- function(x, incomparables = FALSE, ...) {
  duplicated_equal(x, incomparables = incomparables, ...)
}

# `==` and `!=` between node_vecs compare nodes as vctrs::vec_equal() and
# duplicated() do (see node_vec_equal_fields()), recycling as vctrs does; a
# missing node gives NA. A node is never equal to a plain value, so
# comparing with one is an error, pointing to node_values(). As for an
# edge_vec, every other operator errors too: nodes have no order or
# arithmetic of their own, only their values do.
#' @export
Ops.node_vec <- function(e1, e2) {
  hint <- c(i = "Use {.fn node_values} (or {.fn format}) to work with the node values.")
  if (!.Generic %in% c("==", "!=") || missing(e2)) {
    cli::cli_abort(c(
      "{.code {(.Generic)}} is not supported for {.cls node_vec}; only {.code ==} and {.code !=} between {.cls node_vec}s are.",
      hint
    ), call = NULL)
  }
  if (!inherits(e1, "node_vec") || !inherits(e2, "node_vec")) {
    cli::cli_abort(c(
      "Can't compare a {.cls node_vec} with a plain value: nodes compare by graph identity.",
      hint
    ), call = NULL)
  }
  eq <- vctrs::vec_equal(e1, e2)
  if (.Generic == "==") eq else !eq
}

# Exact keys for base match() and %in% (R >= 4.3), one per node, agreeing
# with vctrs::vec_match(): built from node_vec_equal_fields(), so nodes
# match by identity and value (a hyperedge node_vec's by value alone). A
# missing node matches only missing nodes.
# base has no mtfrm() generic before R 4.3, so it's only registered there;
# match() then falls back to its default.
#' @rawNamespace if (getRversion() >= "4.3.0") S3method(mtfrm, node_vec)
#' @exportS3Method NULL
mtfrm.node_vec <- function(x) {
  key <- equality_key(node_vec_equal_fields(x), length(x))
  key[vctrs::vec_detect_missing(x)] <- NA_character_
  key
}

# Ranks for order()/sort()/dplyr::desc() by node value (data frame values
# row-wise, column by column), then by identity, as vctrs::vec_order() sorts
# them.
#' @export
xtfrm.node_vec <- function(x) {
  rank_rows(node_vec_order_fields(x), length(x))
}

#' @export
anyDuplicated.node_vec <- function(x, incomparables = FALSE, ...) {
  first_duplicate(duplicated(x, incomparables = incomparables, ...), ...)
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
  x <- node_vec_compact(x)
  graph <- graph_of(x)
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
  graph <- graph_of(x)
  if (is.null(graph)) {
    return(attr(x, "edges"))
  }
  ends <- graph$edge_endpoints()
  cbind_edge_fields(ends$from, ends$to, attr(x, "edges"))
}

#' @export
unique.node_vec <- function(x, incomparables = FALSE, ...) {
  # Drops repeats of the same node (duplicated.node_vec()) by first
  # occurrence, via [.node_vec's induced-subgraph rules, so edges incident
  # to a dropped repeat are dropped rather than redirected onto the kept
  # node.
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

  sizes <- vapply(xs, length, integer(1))
  node_vec_assemble(xs, src = rep(seq_along(xs), sizes), row = sequence(sizes))
}

# -- Combining node_vecs ---------------------------------------------------
#
# Builds a node_vec whose element `r` is element `row[r]` of input
# `srcs[[src[r]]]` (`row[r]` NA for a missing node). This is the one place
# c(), `[<-` and vctrs' vec_restore() decide what happens to the graph when
# nodes from several inputs end up in one vector. An "input" is one element
# of `srcs`: an argument to c(), `x` or `value` in `[<-`, or the object
# behind one vec_proxy() call (vctrs proxies each argument separately, even
# when two are the same object).
#
# The rule:
#
# 1. Inputs are grouped by graph: the same `graph` uid (same_graph(),
#    which unlike the pointer's address survives a reload) and the same
#    `edges` attribute table. Different graphs are always a disjoint union.
# 2. Within one graph, inputs are taken in order of their first row, and
#    each joins the first copy of the graph that none of its positions are
#    already used in, or else starts a new copy. A copy is one selection
#    from the graph: all its nodes keep the edges between them, as for a
#    single slice. So a position used by two *different* inputs (vec_c(x,
#    x), c(x[1:2], x[2:3])) makes them separate copies (a disjoint union, as
#    c() always was), while inputs that use disjoint positions of the same
#    graph (`if_else(cond, x, x)`, `x[2] <- x[2]`, a no-op rows_patch(),
#    c(x[1:2], x[3:4])) are put back into one graph with every edge between
#    them, including edges crossing between the inputs.
# 3. A row repeated *within* one input (x[c(1, 1)], vec_rep(x, 2),
#    vctrs::vec_slice() with repeats) splits that input as if each repeat
#    came from a further input: the k-th occurrence of each row belongs to
#    the input's k-th part, so by rule 2 each repeat is its own copy of the
#    graph, the same as c(x, x).
# 4. Missing nodes (no position: vec_init(), an NA index) belong to no graph;
#    an input with only missing nodes joins the first copy of whatever else
#    is combined, so it never forces a disjoint union.
# 5. Hyperedge node_vecs have no graph identity: each is its own copy.
#
# With exactly one copy, the result keeps that graph (and so its identity):
# its `node_id` is just the rows' positions. Otherwise every copy is
# compacted to its induced subgraph and the copies are laid out in row order
# (each copy's local positions are mapped to the output rows they fill,
# never offset in group order) into one new graph. Either way every node
# keeps the identity it had in its input (node_vec_origin()), so a node and
# its copies stay equal.
node_vec_assemble <- function(srcs, src, row) {
  n <- length(src)
  directed <- attr(srcs[[1]], "directed")

  # Rule 3: split each input whose rows repeat into one part per occurrence,
  # numbered in order of first row like separate inputs.
  ok <- !is.na(row)
  if (anyDuplicated(cbind(src, row)[ok, , drop = FALSE])) {
    occ <- rep(1L, n)
    occ[ok] <- occurrence(vctrs::vec_group_id(vctrs::new_data_frame(list(src = src[ok], row = row[ok]))))
    part <- vctrs::vec_group_id(vctrs::new_data_frame(list(src = src, occ = occ)))
    srcs <- srcs[src[match(seq_len(max(part)), part)]]
    src <- as.integer(part)
  }
  k <- length(srcs)

  # The common case of a single input (any slice by vctrs): just a slice.
  if (k == 1L) {
    return(srcs[[1L]][row])
  }

  rows_of <- split(seq_len(n), factor(src, levels = seq_len(k)))

  # Node values: each input's selected values in turn, scattered back into
  # row order.
  parts <- lapply(seq_len(k), function(j) slice_rows(node_vec_data(srcs[[j]]), row[rows_of[[j]]]))
  values <- slice_rows(combine_values(parts), order(unlist(rows_of, use.names = FALSE)))

  # Each row's position in its input's graph (NA if missing or hyperedge).
  graphs <- lapply(srcs, graph_of)
  pos <- rep(NA_integer_, n)
  for (j in seq_len(k)) {
    if (!is.null(graphs[[j]]) && length(rows_of[[j]]) > 0L) {
      pos[rows_of[[j]]] <- attr(srcs[[j]], "node_id")[row[rows_of[[j]]]]
    }
  }

  # Each row's identity in its input (NA if missing or hyperedge).
  origin <- list(graph = rep(NA_real_, n), node_id = rep(NA_integer_, n))
  for (j in seq_len(k)) {
    org <- node_vec_origin(srcs[[j]])
    if (!is.null(org) && length(rows_of[[j]]) > 0L) {
      for (f in names(origin)) origin[[f]][rows_of[[j]]] <- org[[f]][row[rows_of[[j]]]]
    }
  }

  # Assign every input with rows to a copy (rules 1-5 above).
  copies <- list() # each: list(src = <representative input>, inputs, used)
  bucket <- rep(NA_integer_, k)
  deferred <- integer() # inputs with only missing nodes
  first_row <- vapply(rows_of, function(r) if (length(r)) r[[1L]] else NA_integer_, integer(1))
  for (j in order(first_row, na.last = NA)) {
    if (is.null(graphs[[j]])) {
      copies[[length(copies) + 1L]] <- list(src = j, used = NULL)
      bucket[j] <- length(copies)
      next
    }
    p <- pos[rows_of[[j]]]
    p <- p[!is.na(p)]
    if (length(p) == 0L) {
      deferred <- c(deferred, j)
      next
    }
    target <- NA_integer_
    for (b in seq_along(copies)) {
      r <- copies[[b]]$src
      if (!is.null(copies[[b]]$used) &&
          same_graph(graphs[[r]], graphs[[j]]) &&
          identical(attr(srcs[[r]], "edges"), attr(srcs[[j]], "edges")) &&
          !any(copies[[b]]$used[p])) {
        target <- b
        break
      }
    }
    if (is.na(target)) {
      copies[[length(copies) + 1L]] <- list(src = j, used = logical(graphs[[j]]$n_nodes()))
      target <- length(copies)
    }
    copies[[target]]$used[p] <- TRUE
    bucket[j] <- target
  }
  if (length(deferred) > 0L) {
    is_graph_copy <- !vapply(copies, function(cp) is.null(cp$used), logical(1))
    if (!any(is_graph_copy)) {
      copies[[length(copies) + 1L]] <- list(src = deferred[[1L]], used = logical(0))
      is_graph_copy <- c(is_graph_copy, TRUE)
    }
    bucket[deferred] <- which(is_graph_copy)[[1L]]
  }

  if (length(copies) == 0L) {
    # Nothing to combine (every input is empty).
    return(new_node_vec(values, directed = directed))
  }

  row_copy <- integer(n)
  for (j in seq_len(k)) row_copy[rows_of[[j]]] <- bucket[j]

  if (length(copies) == 1L) {
    r <- copies[[1L]]$src
    if (is.null(graphs[[r]])) {
      return(srcs[[r]][row])
    }
    out <- new_node_vec_backend(
      values,
      graph = graphs[[r]],
      edges = attr(srcs[[r]], "edges"),
      directed = directed,
      node_id = pos
    )
    return(node_vec_set_origin(out, origin))
  }

  # Several copies: a disjoint union of each copy's induced subgraph, with
  # each copy's local positions mapped to the output rows it fills.
  tables <- lapply(seq_along(copies), function(b) {
    rows_b <- which(row_copy == b)
    r <- copies[[b]]$src
    if (is.null(graphs[[r]])) {
      e <- node_vec_full_edges(srcs[[r]][row[rows_b]])
    } else {
      remap <- graphvec_backend_induced_subgraph(graphs[[r]], pos[rows_b])
      attrs <- attr(srcs[[r]], "edges")[remap$source_edge, , drop = FALSE]
      e <- cbind_edge_fields(remap$from, remap$to, attrs)
    }
    for (role in c("from", "to")) {
      e[[role]] <- if (is.list(e[[role]])) {
        I(lapply(e[[role]], function(v) rows_b[v]))
      } else {
        rows_b[e[[role]]]
      }
    }
    rownames(e) <- NULL
    e
  })

  # Up-cast to a hyperedge column if any copy uses one for this role, so an
  # ordinary and a hyperedge node_vec can still be combined.
  for (col in c("from", "to")) {
    if (any(vapply(tables, function(e) is.list(e[[col]]), logical(1)))) {
      tables <- lapply(tables, function(e) {
        e[[col]] <- as_incidence_list(e[[col]])
        e
      })
    }
  }

  out <- new_node_vec(x = values, edges = rbind_fill(tables), directed = directed)
  if (is.null(attr(out, "graph"))) {
    # A hyperedge result has no graph identity.
    return(out)
  }
  node_vec_set_origin(out, origin)
}

#' @export
rep.node_vec <- function(x, ...) {
  # `[` makes the k-th repeat of each node part of the k-th copy of the
  # graph, so `rep(x, 2)` is `c(x, x)`.
  x[rep(seq_along(x), ...)]
}
