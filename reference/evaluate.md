# evaluate generic

Evaluates functions on a grid

## Usage

``` r
evaluate(f, N, M)
```

## Arguments

- f:

  function

- N:

  integer for rows of output matrix

- M:

  integer for cols of output matrix

## Examples

``` r
f <- function(x, y) x + y
class(f) <- "grid_function"
vals <- evaluate(f, 4, 4)
dim(vals$z)
#> [1] 4 4
```
