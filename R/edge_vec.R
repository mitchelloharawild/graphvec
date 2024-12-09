#' Graph vectors with edge-first design (EXPERIMENTAL)
#' @export
edge_vec <- function(from, to, nodes, key = 1L) {
  inputs <- vec_recycle_common(from = from, to = to)

  key_vars <- tidyselect::eval_select({{key}}, nodes)

  # Check inputs
  vec_assert(inputs$from, integer())
  vec_assert(inputs$to, integer())
  stopifnot(is.data.frame(nodes))

  vctrs::new_rcrd(
    inputs[c("from", "to")],
    nodes = nodes,
    key_vars = key_vars,
    class = "edge_vec"
  )
}

#' @export
format.edge_vec <- function(x, ...){
  key_data <- attr(x, "nodes")[attr(x, "key_vars")]
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
  "graph"
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
