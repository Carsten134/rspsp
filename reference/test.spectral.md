# Resampling Test for Spectral Densities

Compute resampling test for equality or isotropy of spectral densities
or other qualities of the spectral densities. Can be applied to both
vectors and matrices. For more information see DOI: 10.3150/13-BEJ584.

## Usage

``` r
test.spectral(
  x,
  y,
  B,
  alpha,
  hypothesis = "equality",
  h1 = length(x)^(-0.2),
  h2 = length(x)^(-0.2)
)
```

## Arguments

- x:

  first sample. Can be numeric vector or matrix

- y:

  second sample. Can be numeric vector or matrix (but must in it's type
  coincide with x). If isotropy is tested, this is disregarded.

- B:

  Number of iterations for resampling (The more the better). Can be
  numeric but must be a whole number

- alpha:

  Level of significance. Must be numeric value in (0,1)

- hypothesis:

  Can be one of \`"equality", "isotropy", "stationary\`

- h1:

  Kernelbandwidth along first axis defaults to \\(NM)^{-1/5}\\

- h2:

  Kernelbandwidth along second axis defaults to \\(NM)^{-1/5}\\

## Examples

``` r
# simulating grid-data under H0
K0 <- MA_coef_all(.3)
x <- gridMA(25, 25, K0)
y <- gridMA(25, 25, K0)

# applying the test with 100 iterations and significance 5%
JPtest(x, y, 100, .05)
#> Error in JPtest(x, y, 100, 0.05): could not find function "JPtest"
```
