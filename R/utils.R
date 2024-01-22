compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
  lapply(seq_along(.l[[1]]), function(i) {
    lapply(.l, .subset2, i)
  })
}
