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

