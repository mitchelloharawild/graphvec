#' Recursively traverse an object
#'
#' @param x The object to traverse
#' @param .f A function for combining the recursed components
#' @param .g A function applied to the object before recursion
#' @param .h A function applied to the base case
#' @param base The base case for the recursion
#'
#' @keywords internal
traverse <- function(x, .f = list, .g = identity, .h = identity, base = function(.x) is_syntactic_literal(.x) || is_symbol(.x)){
  # base case
  if(base(x))
    return(.h(x))
  # recursive case
  .f(lapply(.g(x), traverse, .f=.f, .g=.g, .h=.h, base=base), .h(x))
}

traverse_list <- function(x,
                          .f = function(.x, .y) as.list(.x),
                          .g = identity,
                          .h = identity,
                          base = function(.x) !is.list(.x)){
  traverse(x, .f=.f, .g=.g, .h=.h, base=base)
}
