# Recursively traverse an object

Recursively traverse an object

## Usage

``` r
traverse(
  x,
  .f = list,
  .g = identity,
  .h = identity,
  base = function(.x) is_syntactic_literal(.x) || is_symbol(.x)
)
```

## Arguments

- x:

  The object to traverse

- .f:

  A function for combining the recursed components

- .g:

  A function applied to the object before recursion

- .h:

  A function applied to the base case

- base:

  The base case for the recursion
