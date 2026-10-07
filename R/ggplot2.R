# -- ggplot2 -----------------------------------------------------------------
#
# ggplot2 picks a default scale from scale_type(), trying the scale_<aes>_<type>
# functions it names in order. Each graph vector names its own type first
# ("node", "edge" or "agg"), so an extension package that defines e.g.
# scale_x_node() takes over, then falls back to a discrete scale labelled by
# format(). That includes numeric node_vecs: nodes have no arithmetic or
# order of their own (Ops.node_vec()), so a continuous scale of their values
# is plotted from node_values():
#
# * Non-position discrete scales (colour, fill, shape, ...) train on
#   levels(x) whenever it isn't NULL (scales:::clevels()) and map with
#   match(as.character(x), limits), so the levels() methods below are all
#   they need: the sorted unique labels, as plain strings. (Training on
#   sort(unique(x)) would keep the graph vector class, whose identity never
#   matches a plain label.)
# * Discrete position scales only treat character, logical and factor data as
#   discrete (ggplot2's is_discrete()), so any other graph vector (an
#   edge_vec, agg_vec, or a numeric or data-frame node_vec) goes through
#   scale_x_graphvec()/scale_y_graphvec() first, which turn it into its labels
#   before training. ggplot2 finds those by name, so only when graphvec is
#   attached.

# Registered dynamically for ggplot2 via zzz.R.
scale_type.node_vec <- function(x) {
  c("node", "graphvec", "discrete")
}

# Registered dynamically for ggplot2 via zzz.R.
scale_type.edge_vec <- function(x) {
  c("edge", "graphvec", "discrete")
}

# Registered dynamically for ggplot2 via zzz.R.
scale_type.agg_vec <- function(x) {
  c("agg", "graphvec", "discrete")
}

# The distinct labels (as.character()) of a graph vector's non-missing
# elements, in its sort order: node and edge values (so a factor's level
# order), then `<aggregated>` last for an agg_vec. These are what a discrete
# ggplot2 scale shows.
graph_vec_levels <- function(x) {
  unique(as.character(sort(x[!is.na(x)])))
}

#' @export
levels.node_vec <- function(x) graph_vec_levels(x)

#' @export
levels.edge_vec <- function(x) graph_vec_levels(x)

#' @export
levels.agg_vec <- function(x) graph_vec_levels(x)

# The levels are only ever those in use, so there are none to drop.
#' @export
droplevels.node_vec <- function(x, ...) x

#' @export
droplevels.edge_vec <- function(x, ...) x

#' @export
droplevels.agg_vec <- function(x, ...) x

#' Discrete position scales for graph vectors
#'
#' The default x and y scales for a `node_vec`, `edge_vec` or `agg_vec` in
#' a ggplot2 plot: a discrete scale with
#' one position per label, as [format()] gives it (so `<aggregated>` is a
#' level of its own), in the vector's sort order. ggplot2 picks them
#' itself when graphvec is attached, unless an extension package provides
#' `scale_x_node()`, `scale_x_edge()` or `scale_x_agg()` (and the `y`
#' versions), which take precedence. Other aesthetics (colour, fill, shape,
#' ...) use ggplot2's own discrete scales, with the same labels. Numeric
#' node values are discrete too: plot `node_values(x)` for a continuous
#' scale.
#'
#' @param ... Passed on to [ggplot2::scale_x_discrete()] or
#'   [ggplot2::scale_y_discrete()].
#'
#' @return A ggplot2 scale.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   e <- edges(node_vec(c("A", "B", "C"), from = 1:2, to = 2:3))
#'   df <- data.frame(y = 1:2)
#'   df$e <- e
#'   ggplot2::ggplot(df, ggplot2::aes(e, y)) +
#'     ggplot2::geom_col() +
#'     scale_x_graphvec()
#' }
#'
#' @export
scale_x_graphvec <- function(...) {
  graphvec_position_scale(ggplot2::scale_x_discrete(...))
}

#' @rdname scale_x_graphvec
#' @export
scale_y_graphvec <- function(...) {
  graphvec_position_scale(ggplot2::scale_y_discrete(...))
}

# A discrete position scale that first turns graph vectors into their labels
# (a factor with levels() as its levels), which ggplot2 then treats as
# ordinary discrete data. Anything else passes through.
graphvec_position_scale <- function(scale) {
  ggplot2::ggproto(NULL, scale, transform = function(self, x) {
    if (inherits(x, c("node_vec", "edge_vec", "agg_vec"))) {
      factor(as.character(x), levels = levels(x))
    } else {
      x
    }
  })
}
