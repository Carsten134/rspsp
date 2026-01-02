# Simulate a lattice AR(1) process

Generates one realisation from an autoregressive process on an \\N
\times M\\ lattice by using the Wold-weight deconstruction.

## Usage

``` r
gridAR(N, M, phi_1, phi_2, phi_3, phi_4 = NULL, wold_size = 50)
```

## Arguments

- N:

  Integer number of rows.

- M:

  Integer number of columns.

- phi_1:

  Horizontal autoregressive coefficient.

- phi_2:

  Vertical autoregressive coefficient.

- phi_3:

  Diagonal autoregressive coefficient.

- phi_4:

  Optional additional diagonal coefficient. If \`NULL\` or \`0\` the
  quarter-plane kernel is used.

- wold_size:

  Size of the Wold-weight lattice, defaults to 50.

## Value

Numeric matrix with \`N\` rows and \`M\` columns.

## Examples

``` r
set.seed(1)
x <- gridAR(8, 8, 0.2, 0.1, 0.05)
dim(x)
#> [1] 8 8
```
