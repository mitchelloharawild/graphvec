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
#' frame of node attributes). Every `from`/`to` position must be one of its
#' elements (rows). The default, a zero-column data frame, means no node
#' data: the nodes are then every position up to the largest one in `from`
#' and `to`.
#' @param directed A single logical value: is incidence ordered (`from` -> `to`)
#' or symmetric?
#'
#' @return An `edge_vec` object.
#'
#' @section Slicing, combining and equality:
#' Every `edge_vec` belongs to a graph, which slicing keeps: `e[i]` selects
#' edges of the same graph, between the same nodes.
#'
#' Combining edge_vecs, with [c()], `[<-`, `vctrs::vec_c()`,
#' `dplyr::bind_rows()`, `dplyr::rows_patch()` and the like, shares the
#' graph between inputs of the same graph: their edges keep pointing at the
#' same nodes, and the nodes aren't copied, so `c(e, e)` has the same nodes
#' as `e`. Edge_vecs of different graphs are combined as a disjoint union of
#' their graphs (each graph's nodes once, in order of first appearance); the
#' edges always stay in the order they're combined in.
#'
#' Two edges are equal when they are edges of the same graph between the
#' same node positions, with the same edge attributes, whether or not
#' there is node data. This is what [duplicated()], [unique()],
#' `vctrs::vec_in()`, joins, `dplyr::distinct()` and `dplyr::count()` use,
#' so `e[2]` matches the second edge of `e` (or of `c(e, e)`), but edges of
#' two separately built edge_vecs never match, even with identical labels:
#' to match those by label, compare `format(e)` explicitly, e.g.
#' `dplyr::mutate(df, key = format(e))` before joining `by = "key"`.
#' Edge_vecs sort by the node values at each end (then the edge
#' attributes, then node positions to break ties). Hyperedges have no
#' graph identity: they compare by node values and always combine as a
#' disjoint union.
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
  graph <- graph_of(x)
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
  graph <- graph_of(x)
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
  # edge_vecs of different graphs without node data is a disjoint union like
  # any other. With node data, every position must be one of its rows.
  infer_nodes <- is.data.frame(nodes) && ncol(nodes) == 0L
  if (infer_nodes) {
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
  check_edge_positions(from, to, if (infer_nodes) Inf else NROW(nodes))
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
#' unaffected and the slice keeps the same graph, which is what lets
#' [c()] put slices of the same graph back together without copying their
#' nodes (see [edge_vec()]).
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
  graph <- graph_of(x)

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
  x[element_position(x, i)]
}

# Assigning edges from another edge_vec combines them like c() and
# vctrs::vec_assign(): edges of `x`'s own graph (such as `x[2] <- x[3]`)
# keep pointing at `x`'s nodes, while edges of another graph bring their
# nodes along as a disjoint union, appended to `x`'s.
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
  n <- length(x)
  from_value <- !is.na(pos) & pos > n
  edge_vec_assemble(
    list(x, value),
    src = ifelse(from_value, 2L, 1L),
    row = ifelse(from_value, pos - n, pos)
  )
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
  graph <- graph_of(x)
  if (!is.null(graph)) {
    length(attr(x, "edge_id"))
  } else {
    length(edge_vec_data(x)[["from"]]) # `from` is aligned 1:1 with edges
  }
}

# Combining edge_vecs groups the inputs by graph:
#
# - Inputs with the same backing graph (the same `graph` uid, see
#   same_graph(), and the same `nodes`) share it: their edges
#   keep pointing at the same nodes, with no offset and one node table. So
#   `c(e, e)`, `c(e[2], e)`, a no-op rows_patch() or `x[2] <- x[2]` never
#   copy the nodes, and positions stay comparable, which is what edge
#   equality (graph + positions) and joins rely on.
# - Different graphs are a disjoint union: one copy of each graph's nodes,
#   in order of first appearance, and each input's positions offset to its
#   graph's copy. Rows always stay in input order.
# - An input with only missing edges belongs to no graph, and joins the
#   first graph of whatever else is combined. (An input with no edges at
#   all still brings its graph's nodes, which may be isolated nodes.)
# - Hyperedge edge_vecs have no graph identity: each is its own copy.
#
# Unlike node_vecs, repeats never split a graph: the elements are edges, and
# two copies of an edge are still edges between the same nodes.
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

  # Group the inputs by graph; `reps` holds the first input of each group.
  graphs <- lapply(xs, graph_of)
  group <- rep(NA_integer_, length(xs))
  reps <- integer()
  agnostic <- integer()
  for (j in seq_along(xs)) {
    g <- graphs[[j]]
    if (!is.null(g) && length(xs[[j]]) > 0L && all(is.na(attr(xs[[j]], "edge_id")))) {
      agnostic <- c(agnostic, j)
      next
    }
    hit <- NA_integer_
    if (!is.null(g)) {
      for (r in seq_along(reps)) {
        x_r <- xs[[reps[[r]]]]
        if (same_graph(graphs[[reps[[r]]]], g) && identical(attr(x_r, "nodes"), attr(xs[[j]], "nodes"))) {
          hit <- r
          break
        }
      }
    }
    if (is.na(hit)) {
      reps <- c(reps, j)
      hit <- length(reps)
    }
    group[j] <- hit
  }
  if (length(agnostic) > 0L) {
    has_graph <- !vapply(reps, function(r) is.null(graphs[[r]]), logical(1))
    if (!any(has_graph)) {
      reps <- c(reps, agnostic[[1L]])
      has_graph <- c(has_graph, TRUE)
    }
    group[agnostic] <- which(has_graph)[[1L]]
  }

  if (length(reps) == 1L && !is.null(graphs[[reps]])) {
    # One graph: share it. Only the attribute table and edge ids combine.
    edge_id <- unlist(lapply(xs, attr, "edge_id"), use.names = FALSE)
    if (is.null(edge_id)) edge_id <- integer()
    return(new_edge_vec_backend(
      bind_edge_attrs(lapply(xs, edge_vec_attrs_df), length(edge_id)),
      nodes = attr(xs[[reps]], "nodes"),
      directed = directed,
      graph = graphs[[reps]],
      edge_id = edge_id
    ))
  }

  # Disjoint union of the groups: one copy of each group's nodes, and every
  # input's from/to positions offset to its group's copy.
  node_sizes <- vapply(reps, function(r) NROW(attr(xs[[r]], "nodes")), integer(1))
  offsets <- cumsum(c(0L, utils::head(node_sizes, -1L)))

  fields <- lapply(seq_along(xs), function(j) {
    f <- edge_vec_fields_df(xs[[j]])
    offset <- offsets[[group[j]]]
    f[["from"]] <- offset_incidence(f[["from"]], offset)
    f[["to"]] <- offset_incidence(f[["to"]], offset)
    f
  })

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
    nodes = combine_values(lapply(reps, function(r) attr(xs[[r]], "nodes"))),
    directed = directed
  )
}

# An edge_vec's attribute columns (everything but from/to) as a genuine data
# frame with one row per edge, even with no columns at all.
edge_vec_attrs_df <- function(x) {
  body <- edge_vec_data(x)
  if (is.null(attr(x, "graph"))) {
    body <- unclass(body)
    return(attrs_frame(body[setdiff(names(body), c("from", "to"))], length(x)))
  }
  attr(body, "row.names") <- .set_row_names(length(x))
  class(body) <- "data.frame"
  body
}

# Row-binds attribute tables, padding missing columns with NA, keeping the
# row count `n` when none of them has any column.
bind_edge_attrs <- function(dfs, n) {
  if (all(vapply(dfs, length, integer(1)) == 0L)) {
    return(attrs_frame(list(), n))
  }
  out <- rbind_fill(dfs)
  rownames(out) <- NULL
  out
}

# Rebuilds an edge_vec whose element `r` is element `row[r]` of input
# `srcs[[src[r]]]` (`row[r]` NA for a missing edge): each input's rows are
# sliced, combined with c() (which shares a graph between same-graph inputs,
# see above) in order of each input's first row, then scattered back into
# row order. Used by `[<-` and vctrs' vec_restore().
edge_vec_assemble <- function(srcs, src, row) {
  if (length(srcs) == 1L) {
    return(srcs[[1L]][row])
  }
  rows_of <- split(seq_along(src), factor(src, levels = seq_along(srcs)))
  first_row <- vapply(rows_of, function(r) if (length(r)) r[[1L]] else NA_integer_, integer(1))
  used <- order(first_row, na.last = NA)
  parts <- lapply(used, function(j) srcs[[j]][row[rows_of[[j]]]])
  out <- do.call(c, parts)
  out[order(unlist(rows_of[used], use.names = FALSE))]
}

#' @rdname reorient
#' @export
edges.edge_vec <- function(x, ...) {
  x
}

#' @rdname reorient
#' @export
nodes.edge_vec <- function(x, ...) {
  graph <- graph_of(x)
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
  if (!is.null(graph)) {
    # A missing edge (no endpoints, e.g. from vctrs::vec_init()) joins no
    # nodes, so it has no place among the node_vec's edges.
    edge_table <- edge_table[!is.na(attr(x, "edge_id")), , drop = FALSE]
  }

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

# Edges are equal when they are edges of the same graph between the same
# node positions in each role, with the same edge attributes (see
# edge_vec_equal_fields()).
#' @export
duplicated.edge_vec <- function(x, incomparables = FALSE, ...) {
  fields <- edge_vec_equal_fields(x)
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
anyDuplicated.edge_vec <- function(x, incomparables = FALSE, ...) {
  first_duplicate(duplicated(x, incomparables = incomparables, ...), ...)
}

# `==` and `!=` compare edges as vctrs::vec_equal() does (see
# edge_vec_equal_fields()), recycling as vctrs does; a missing edge gives NA.
# Edges have no order or arithmetic, so every other operator errors.
#' @export
Ops.edge_vec <- function(e1, e2) {
  if (!.Generic %in% c("==", "!=") || missing(e2)) {
    cli::cli_abort(
      "{.code {(.Generic)}} is not supported for {.cls edge_vec}; only {.code ==} and {.code !=} are.",
      call = NULL
    )
  }
  eq <- vctrs::vec_equal(e1, e2)
  if (.Generic == "==") eq else !eq
}

# Exact keys for base match() and %in% (R >= 4.3), one per edge, agreeing
# with vctrs::vec_match(): built from edge_vec_equal_fields(), so edges
# match by graph and node positions (and edge attributes). A missing edge
# matches only missing edges.
# base has no mtfrm() generic before R 4.3, so it's only registered there;
# match() then falls back to its default.
#' @rawNamespace if (getRversion() >= "4.3.0") S3method(mtfrm, edge_vec)
#' @exportS3Method NULL
mtfrm.edge_vec <- function(x) {
  key <- equality_key(edge_vec_equal_fields(x), length(x))
  key[is.na(x)] <- NA_character_
  key
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
  missing <- lapply(edge_vec_equal_fields(x), field_is_missing)
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

# The per-edge values to sort edges by: edge_vec_value_fields(), with each
# hyperedge role's node sets replaced by their lexicographic rank within `x`.
# Shared by xtfrm() and vctrs' order proxy so the two agree.
#
# Node values come first so that edges sort by label, for display; for an
# ordinary edge_vec the node positions follow as a tie-break, so that two
# edges only tie when they are equal (edge_vec_equal_fields()): the order
# proxy is also what vctrs matches join keys with.
edge_vec_order_fields <- function(x) {
  fields <- edge_vec_value_fields(x)
  for (role in c("from", "to")) {
    if (is.list(fields[[role]]) && !is.data.frame(fields[[role]])) {
      fields[[role]] <- incidence_set_rank(attr(x, "nodes"), edge_vec_endpoints(x)[[role]])
    }
  }
  if (!is.null(attr(x, "graph"))) {
    ends <- edge_vec_endpoints(x)
    fields <- c(fields, list(.from_pos = ends$from, .to_pos = ends$to))
  }
  fields
}

# The per-edge fields that make two edges equal. For an ordinary edge_vec:
# its graph (the backend's uid(), its graph identity; see same_graph()),
# the node positions in each role, then the edge attributes. So two edges
# are equal exactly when they are the same graph's edges between the same
# nodes (positions, in order, even when undirected) with the same
# attributes, whether or not `nodes` holds node data; edges of different
# graphs never are, even with identical labels. A missing edge is missing in
# every field. Hyperedges have no graph identity and keep comparing by value
# (edge_vec_value_fields()).
edge_vec_equal_fields <- function(x) {
  graph <- graph_of(x)
  if (is.null(graph)) {
    return(edge_vec_value_fields(x))
  }
  ends <- edge_vec_endpoints(x)
  uid <- rep(graph$uid(), length(x))
  uid[is.na(attr(x, "edge_id"))] <- NA
  c(
    list(.graph = uid, .from = ends$from, .to = ends$to),
    as.list(edge_vec_attrs_df(x))
  )
}

# Ranks for order()/sort()/dplyr::desc(): by the node values at each end,
# then the edge attributes, as vctrs::vec_order() sorts them.
#' @export
xtfrm.edge_vec <- function(x) {
  rank_rows(edge_vec_order_fields(x), length(x))
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
