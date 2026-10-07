# node_vec() errors on an edge attribute named like a misspelt option

    Code
      node_vec(c("A", "B"), 1L, 2L, directd = FALSE)
    Condition
      Error in `node_vec()`:
      ! Edge attribute `directd` looks like a misspelt option.
      i Did you mean `directed`?
      i Edge attributes can't have names this close to `directed`.

