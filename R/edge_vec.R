#' Graph vector along edges
#'
#' An `edge_vec` is a vector of graph edges with associated node data stored as
#' attributes.
#' 
#' @param from Integer vector of 'from' node indices.
#' @param to Integer vector of 'to' node indices.
#' @param nodes Vector of node data. The vector size should be at least the
#' maximum value in `from` and `to`.
#'   
#' @examples
#' g <- edge_vec(
#'   from = c(1L, 2L, 1L, 3L),
#'   to = c(2L, 3L, 3L, 1L),
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
#' 
#' @export
edge_vec <- function(from = integer(), to = integer(), nodes = data.frame()) {
  inputs <- vec_recycle_common(from = from, to = to)

  # key_vars <- tidyselect::eval_select({{key}}, nodes)

  # Check inputs
  vec_assert(inputs$from, integer())
  vec_assert(inputs$to, integer())
  stopifnot(is.data.frame(nodes))

  new_edge_vec(
    from = inputs$from,
    to = inputs$to,
    nodes = nodes#,
    # key_vars = key_vars
  )
}

#' Constructor function for edge_vec
#' 
#' @param from Integer vector of 'from' node indices.
#' @param to Integer vector of 'to' node indices.
#' @param nodes Data frame of node data.
# #' @param key_vars Integer vector of column indices in `nodes` that represent
#' 
#' @export
new_edge_vec <- function(from = integer(), to = integer(), nodes = data.frame()) {#, key_vars = integer()) {
  vctrs::new_rcrd(
    list(from = from, to = to),
    nodes = nodes,
    # key_vars = key_vars,
    class = "edge_vec"
  )
}

#' @export
format.edge_vec <- function(x, ...){
  key_data <- attr(x, "nodes")#[attr(x, "key_vars")]
  sprintf(
    "[%s]->[%s]",
    do.call(paste, c(key_data[field(x, "from"),,drop=FALSE], sep = ":")),
    do.call(paste, c(key_data[field(x, "to"),,drop=FALSE], sep = ":"))
  )
}

#' @export
nodes.edge_vec <- function(x, ...) {
  attr(x, "nodes")
}

#' @rdname aggregation-vctrs
#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 edge_vec
#' @export
vec_ptype2.edge_vec <- function(x, y, ...) UseMethod("vec_ptype2.edge_vec", y)

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.edge_vec.default <- function(x, y, ...) edge_vec()

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.edge_vec.character <- function(x, y, ...) edge_vec()

#' @rdname aggregation-vctrs
#' @method vec_cast edge_vec
#' @export
vec_cast.edge_vec <- function(x, to, ...) UseMethod("vec_cast.edge_vec")
#' @rdname aggregation-vctrs
#' @export
vec_cast.edge_vec.edge_vec <- function(x, to, ...) {
  # x <- vec_proxy(x)
  # if(all(x$agg)) x$x <- vec_rep(vec_cast(NA, vec_proxy(to)$x), length(x$x))
  vec_restore(x, to)
}
#' @rdname aggregation-vctrs
#' @export
vec_cast.edge_vec.default <- function(x, to, ...) edge_vec(x)
#' @rdname aggregation-vctrs
#' @export
vec_cast.character.edge_vec <- function(x, to, ...) format(x)

#' @rdname aggregation-vctrs
#' @export
vec_ptype_abbr.edge_vec <- function(x, ...) {
  "edges"
  # class(x) <- class(x)[-1]
  # paste0(vec_ptype_abbr(x, ...), "'")
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.edge_vec <- function(x, pattern){
  utils::.DollarNames(vec_data(x), pattern)
}

#' @export
`$.edge_vec` <- function(x, name){
  attr(x, "nodes")[field(x, name),,drop=FALSE]
}

