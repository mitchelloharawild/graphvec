#' Create an aggregation vector
#'
#' An aggregation vector is a special type of [`node_vec()`] consisting of a
#' single parent (the 'aggregated' value) and its children. Aggregated values
#' are identified by a logical vector passed to the `aggregated` argument, and
#' disaggregated values are provided in `x`. Aggregated values are displayed
#' as `<aggregated>` by default.
#'
#' @details
#' `agg_vec` represents one *dimension* of aggregation as a two-level chain
#' (disaggregated values below their shared `<aggregated>` total). It
#' relies on matching key values across columns, not a stored edge, to find
#' each row's parent (see [agg_df()]). This is the same construction as
#' SQL's `CUBE`/`ROLLUP`/`GROUPING SETS` (Gray, Bosworth, Layman & Pirahesh,
#' 1996), where `<aggregated>` plays the role of their sentinel `NULL`
#' marking "summed over this column"; several `agg_vec` columns combined in
#' an [`agg_df()`] produce exactly the "lattice of cuboids" of Harinarayan,
#' Rajaraman & Ullman (1996).
#'
#' Because it infers parents rather than storing them, `agg_vec`/`agg_df`
#' only support the nested (hierarchical) and crossed (grouped) dimension
#' structures a symbolic formula like `Purpose * (State / Region)` can
#' produce: always at least a meet-semilattice, and a full distributive
#' lattice when every pair of combined columns is fully crossed. They
#' cannot represent a hypergraph, that is, an aggregate whose value is
#' jointly determined by one *specific, tagged* group of children (a
#' directed hyperedge/AND-arc), as opposed to an ordinary total reachable
#' by matching key values. A well-formed `agg_vec` has at most one row per
#' distinct combination of aggregated/disaggregated values; a duplicate
#' combination (e.g. two consecutive `<aggregated>` rows with no
#' disaggregated row between them) can't be resolved to a single parent,
#' and is reported as a hyperedge by [igraph::as.igraph()] rather than
#' silently resolved to one. For arbitrary graphs, such as explicit
#' hyperedges or aggregation structures with no shared bottom series across
#' pathways (divergent groups), use [node_vec()]/[edge_vec()] directly
#' instead.
#'
#' @references
#' Gray, J., Bosworth, A., Layman, A., & Pirahesh, H. (1996). Data Cube: A
#' Relational Aggregation Operator Generalizing Group-By, Cross-Tab, and
#' Sub-Totals. *ICDE*.
#'
#' Harinarayan, V., Rajaraman, A., & Ullman, J. D. (1996). Implementing
#' Data Cubes Efficiently. *SIGMOD*.
#'
#' @param x The vector of values.
#' @param aggregated A logical vector, the same length as `x`, to identify
#'   which values are `<aggregated>`.
#'
#' @return An `agg_vec` object.
#'
#' @examples
#' agg_vec(
#'   x = c(NA, "A", "B"),
#'   aggregated = c(TRUE, FALSE, FALSE)
#' )
#'
#' @export
agg_vec <- function(x = character(), aggregated = logical(NROW(x))){
  is_agg <- is_aggregated(x)
  if (inherits(x, "agg_vec")) x <- agg_vec_expand(x)
  if (!is.logical(aggregated) || length(aggregated) != NROW(x)) {
    stop("`aggregated` must be a logical vector the same length as `x`.", call. = FALSE)
  }
  is_agg <- is_agg | aggregated
  new_agg_vec(x[!is_agg], which(is_agg))
}

# x: disaggregated values. agg_pos: their aggregated positions in the full
# vector. x is wrapped in a list so its class/attributes aren't overwritten.
new_agg_vec <- function(x, agg_pos) {
  structure(list(x), class = "agg_vec", agg_pos = agg_pos)
}

# The disaggregated values, unwrapped.
agg_vec_values <- function(x) {
  .subset2(x, 1L)
}

# Full-length logical mask: TRUE at each aggregated position.
agg_vec_is_agg <- function(x) {
  out <- logical(length(agg_vec_values(x)) + length(attr(x, "agg_pos")))
  out[attr(x, "agg_pos")] <- TRUE
  out
}

# Full-length vector: real values at disaggregated positions, NA at aggregated ones.
agg_vec_expand <- function(x) {
  is_agg <- agg_vec_is_agg(x)
  vals <- agg_vec_values(x)
  out <- vals[rep(NA_integer_, length(is_agg))]
  out[!is_agg] <- vals
  out
}

#' @export
format.agg_vec <- function(x, ..., agg_chr = "<aggregated>"){
  is_agg <- agg_vec_is_agg(x)
  out <- character(length(is_agg))
  out[is_agg] <- agg_chr
  out[!is_agg] <- format(agg_vec_values(x), ...)
  out
}

#' @export
print.agg_vec <- function(x, ...) {
  cat(sprintf("<agg_vec[%d]>\n", length(x)))
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

# Registered dynamically for pillar via zzz.R.
pillar_shaft.agg_vec <- function(x, ...) {
  if(requireNamespace("crayon", quietly = TRUE)){
    agg_chr <- crayon::style("<aggregated>", crayon::make_style("#999999", grey = TRUE))
  }
  else{
    agg_chr <- "<aggregated>"
  }

  out <- format(x, agg_chr = agg_chr)

  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 10)
}

# Registered dynamically for pillar via zzz.R; abbreviated type header, e.g. "chr*".
type_sum.agg_vec <- function(x, ...) {
  paste0(pillar::type_sum(agg_vec_values(x)), "*")
}

#' @export
length.agg_vec <- function(x) {
  length(agg_vec_values(x)) + length(attr(x, "agg_pos"))
}

#' @export
`[.agg_vec` <- function(x, i, ...) {
  is_agg <- agg_vec_is_agg(x)[i]
  vals <- agg_vec_expand(x)[i]
  new_agg_vec(vals[!is_agg], which(is_agg))
}

#' @export
`[[.agg_vec` <- function(x, i, ...) {
  check_scalar_index(i)
  x[i]
}

# Element-wise assignment, as for the full-length vector: `value` may be an
# agg_vec (so `<aggregated>` can be assigned) or a plain vector of values,
# which is never `<aggregated>`. Base recycling and coercion rules apply.
#' @export
`[<-.agg_vec` <- function(x, i, value) {
  vals <- agg_vec_expand(x)
  is_agg <- agg_vec_is_agg(x)
  value_agg <- is_aggregated(value)
  if (inherits(value, "agg_vec")) value <- agg_vec_expand(value)
  if (missing(i)) {
    vals[] <- value
    is_agg[] <- value_agg
  } else {
    vals[i] <- value
    is_agg[i] <- value_agg
  }
  # Positions added past the end (base `[<-` extends) are missing values.
  is_agg[is.na(is_agg)] <- FALSE
  new_agg_vec(vals[!is_agg], which(is_agg))
}

#' @export
`[[<-.agg_vec` <- function(x, i, value) {
  check_scalar_index(i)
  if (length(value) != 1L) {
    stop("`value` must be a single value.", call. = FALSE)
  }
  x[i] <- value
  x
}

#' @export
as.list.agg_vec <- function(x, ...) {
  lapply(seq_along(x), function(i) x[i])
}

#' @export
c.agg_vec <- function(...) {
  xs <- list(...)
  sizes <- vapply(xs, length, integer(1))
  offsets <- cumsum(c(0L, utils::head(sizes, -1L)))
  new_agg_vec(
    x = do.call(c, lapply(xs, agg_vec_values)),
    agg_pos = do.call(c, Map(function(x, offset) attr(x, "agg_pos") + offset, xs, offsets))
  )
}

#' @export
`==.agg_vec` <- function(e1, e2){
  e1_agg <- inherits(e1, "agg_vec")
  e2_agg <- inherits(e2, "agg_vec")

  if(!e1_agg || !e2_agg){
    x <- list(e1,e2)[[which(!c(e1_agg, e2_agg))]]
    x <- agg_vec(x, aggregated = logical(NROW(x)))
    if(!e1_agg) e1 <- x else e2 <- x
  }

  x1 <- agg_vec_expand(e1)
  x2 <- agg_vec_expand(e2)
  val_eq <- (x1 == x2) | (is.na(x1) & is.na(x2))
  val_eq[is.na(val_eq)] <- FALSE
  (agg_vec_is_agg(e1) & agg_vec_is_agg(e2)) | val_eq
}

#' @export
`!=.agg_vec` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
is.na.agg_vec <- function(x) {
  is.na(agg_vec_expand(x)) & !agg_vec_is_agg(x)
}

#' @export
rep.agg_vec <- function(x, ...) {
  x[rep(seq_along(x), ...)]
}

#' @export
as.character.agg_vec <- function(x, ...) {
  trimws(format(x, ...))
}

#' @export
duplicated.agg_vec <- function(x, incomparables = FALSE, ...) {
  is_agg <- agg_vec_is_agg(x)
  vals <- agg_vec_expand(x)
  # Disaggregated duplicates match on value; `<aggregated>` ones on the flag alone.
  dup <- logical(length(is_agg))
  dup[is_agg] <- duplicated(is_agg[is_agg])
  dup[!is_agg] <- duplicated(vals[!is_agg], incomparables = incomparables, ...)
  dup
}

#' @export
unique.agg_vec <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x, incomparables = incomparables, ...)]
}

# Ranks for order()/sort(): disaggregated values by their own order, with
# `<aggregated>` after all of them.
#' @export
xtfrm.agg_vec <- function(x) {
  is_agg <- agg_vec_is_agg(x)
  out <- numeric(length(is_agg))
  out[!is_agg] <- rank(xtfrm(agg_vec_values(x)), na.last = "keep", ties.method = "min")
  out[is_agg] <- sum(!is_agg) + 1
  out
}

# 1-column special case: every aggregated position is a parent of every
# non-aggregated position (a single star).
#' @rdname reorient
#' @export
nodes.agg_vec <- function(x, ...) {
  nodes(new_agg_df(list(value = x)))
}

#' @rdname reorient
#' @export
edges.agg_vec <- function(x, ...) {
  edges(new_agg_df(list(value = x)))
}

# #' @importFrom dplyr recode
# #' @export
# recode.agg_vec <- function(.x, ...) {
#   field(.x, "x") <- recode(field(.x, "x"), ...)
#   .x
# }

#' Is the element an aggregation of smaller data
#'
#' @param x An object.
#' @return A logical vector indicating which elements are aggregated.
#'
#' @seealso [`agg_vec()`]
#'
#' @examples
#' v <- agg_vec(c(NA, "A", "B"), c(TRUE, FALSE, FALSE))
#' is_aggregated(v)
#'
#' @export
is_aggregated <- function(x){
  if(!inherits(x, "agg_vec")){
    logical(NROW(x))
  } else {
    agg_vec_is_agg(x)
  }
}
