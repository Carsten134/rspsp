## `rspsp`: Spatio spectral analysis in R

This package provides tools for testing the equality of spectral
densities on a 2D grid.

It is based on a resampling technique by Prof. Dr. Jentsch and
Prof. Dr. Markus Pauly [DOI](http://doi.org/10.3150/13-BEJ584).

### Installing from GitHub

This package is not listed on CRAN. You will have to install it from
github

``` r
library(devtools)
devtools::install_github("https://github.com/Carsten134/rspsp")
```

### Usage

You can simulate a spatial MA process under H_1 and the method will
detect, that the spatial dependencies are different.

Just like the Jentsch, Pauly 2015 the test statistic is named
\varphi_n^\*

``` r
# simulate 20 x 20 MA grid under H1
x <- gridMA(20, 20, MA_coef_col(.7))
y <- gridMA(20, 20, MA_coef_row(.7))

# apply test statistic
phi_n_star(x,y, 100, .05)
```
