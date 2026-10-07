# an edge_vec errors on operators other than == and !=

    Code
      e < e
    Condition
      Error:
      ! `<` is not supported for <edge_vec>; only `==` and `!=` are.
    Code
      e + 1
    Condition
      Error:
      ! `+` is not supported for <edge_vec>; only `==` and `!=` are.
    Code
      !e
    Condition
      Error:
      ! `!` is not supported for <edge_vec>; only `==` and `!=` are.

# edge_vec() errors on an edge attribute named like a misspelt option

    Code
      edge_vec(1L, 2L, node = c("a", "b"))
    Condition
      Error in `edge_vec()`:
      ! Edge attribute `node` looks like a misspelt option.
      i Did you mean `nodes`?
      i Edge attributes can't have names this close to `nodes` or `directed`.
    Code
      edge_vec(1L, 2L, directd = FALSE)
    Condition
      Error in `edge_vec()`:
      ! Edge attribute `directd` looks like a misspelt option.
      i Did you mean `directed`?
      i Edge attributes can't have names this close to `nodes` or `directed`.
    Code
      new_edge_vec(1L, 2L, dir = FALSE)
    Condition
      Error in `new_edge_vec()`:
      ! Edge attribute `dir` looks like a misspelt option.
      i Did you mean `directed`?
      i Edge attributes can't have names this close to `nodes` or `directed`.

