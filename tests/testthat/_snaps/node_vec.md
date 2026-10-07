# == and != on node_vecs compare as vec_equal() does

    Code
      n == "A"
    Condition
      Error:
      ! Can't compare a <node_vec> with a plain value: nodes compare by graph identity.
      i Use `node_values()` (or `format()`) to work with the node values.
    Code
      "A" != n
    Condition
      Error:
      ! Can't compare a <node_vec> with a plain value: nodes compare by graph identity.
      i Use `node_values()` (or `format()`) to work with the node values.
    Code
      n < n
    Condition
      Error:
      ! `<` is not supported for <node_vec>; only `==` and `!=` between <node_vec>s are.
      i Use `node_values()` (or `format()`) to work with the node values.
    Code
      n + 1
    Condition
      Error:
      ! `+` is not supported for <node_vec>; only `==` and `!=` between <node_vec>s are.
      i Use `node_values()` (or `format()`) to work with the node values.
    Code
      !n
    Condition
      Error:
      ! `!` is not supported for <node_vec>; only `==` and `!=` between <node_vec>s are.
      i Use `node_values()` (or `format()`) to work with the node values.

# node_vec() errors on an edge attribute named like a misspelt option

    Code
      node_vec(c("A", "B"), 1L, 2L, directd = FALSE)
    Condition
      Error in `node_vec()`:
      ! Edge attribute `directd` looks like a misspelt option.
      i Did you mean `directed`?
      i Edge attributes can't have names this close to `directed`.

