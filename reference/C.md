# Covariance function of a grid MA process

Computes the covariance of a grid MA process: \\Cov(X\_{s+k, t+l},
X\_{s,t})\\

## Usage

``` r
C(k, l, K)
```

## Arguments

- k:

  integer offset on row

- l:

  integer offset on column

- K:

  Coefficient matrix of "lag" polynomial

## Value

numeric: value of \\C(k,l) = Cov(X\_{s+k,t+l},X\_{s,t})\\
