# -- ggplot2 -----------------------------------------------------------------
#
# graphvec has no ggplot2 code of its own beyond the scale_type() methods.
# ggplot2 picks a default scale from scale_type(), trying the
# scale_<aes>_<type> functions it names in order. Each graph vector names its
# own type first ("node", "edge" or "agg"), so an extension package that
# defines e.g. scale_x_node() or scale_colour_edge() takes over, then falls
# back to one of ggplot2's own scales:
#
# * Non-position discrete scales (colour, fill, shape, ...) train on
#   levels(x) whenever it isn't NULL (scales:::clevels(), and
#   scales:::is.discrete() checks it too) and map with
#   match(as.character(x), limits), so the levels() methods below are all
#   they need: plain string labels. (Training on sort(unique(x)) would keep
#   the graph vector class, whose identity never matches a plain label.)
# * Discrete position scales only treat character, logical and factor data
#   as discrete (ggplot2's is_discrete()), so on x/y only node_vecs of such
#   values work; an edge_vec, agg_vec, or numeric or data-frame node_vec
#   needs `format(x)` or `node_values(x)` there, or an extension package.
# * Continuous scales censor with `x < limit`, which Ops.node_vec() rejects,
#   so every "continuous" in the values' types becomes "discrete" (numeric
#   nodes then get discrete colours). Types named before it, such as "date"
#   and "datetime", are still tried first, and their scales work.

# Registered dynamically for ggplot2 via zzz.R. The node values' own scale
# types, with "discrete" for "continuous"; a data frame of values has no
# scale type of its own, so is discrete.
scale_type.node_vec <- function(x) {
  values <- node_values(x)
  if (is.data.frame(values)) {
    return(c("node", "discrete"))
  }
  type <- ggplot2::scale_type(values)
  type[type == "continuous"] <- "discrete"
  c("node", unique(type))
}

# Registered dynamically for ggplot2 via zzz.R.
scale_type.edge_vec <- function(x) {
  c("edge", "discrete")
}

# Registered dynamically for ggplot2 via zzz.R.
scale_type.agg_vec <- function(x) {
  c("agg", "discrete")
}

# The distinct labels (as.character()) of a graph vector's non-missing
# elements, in its sort order: node and edge values (so a factor's level
# order), then `<aggregated>` last for an agg_vec. These are what a discrete
# ggplot2 scale shows (see above).
graph_vec_levels <- function(x) {
  unique(as.character(sort(x[!is.na(x)])))
}

# A node_vec of values with levels of their own (a factor) keeps them, as it
# keeps behaving like what it wraps.
#' @export
levels.node_vec <- function(x) {
  lev <- levels(node_vec_data(x))
  if (is.null(lev)) graph_vec_levels(x) else lev
}

#' @export
levels.edge_vec <- function(x) graph_vec_levels(x)

#' @export
levels.agg_vec <- function(x) graph_vec_levels(x)

# Otherwise the levels are only ever those in use, so there are none to drop.
#' @export
droplevels.node_vec <- function(x, ...) {
  values <- node_vec_data(x)
  if (is.null(levels(values))) {
    return(x)
  }
  node_vec_with_values(x, droplevels(values, ...))
}

#' @export
droplevels.edge_vec <- function(x, ...) x

#' @export
droplevels.agg_vec <- function(x, ...) x
