# Simulate a spatial MA process

To validate the implementation we need data generated under both \\H_0\\
and the alternative. We therefore simulate a simple spatial \\MA(q)\\
process. The idea is to interpret lag coefficients as distance based
influence on \\X\_{ij}\\, which can be implemented via a 2D convolution.
The resulting process is stationary with an isotropic covariance
function.

## Usage

``` r
gridMA(N, M, K, distribution = "normal")
```

## Arguments

- N:

  rows

- M:

  columns of grid

- K:

  numeric matrix of kernel weights

- distribution:

  Type of distribution must be one of "normal", "uniform", "cauchy",
  "chisq"

## Value

matrix with N rows and M columns

## Examples

``` r
set.seed(1)
K <- MA_coef_all(0.3)
x <- gridMA(8, 8, K)
dim(x)
#> [1] 8 8
```
