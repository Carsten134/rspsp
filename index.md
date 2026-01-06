# `rspsp`: Spatio Spectral Analysis in R

Lattice data (meaning measurements taken on a raster) are abundantly
available in environmental sciences, geology, astrophysics, and surface
imagery. We developed this package to offer tools to inspect the
spectral properties of such data.

Inspecting the spectral properties of lattice data can help get
supporting evidence for modeling assumptions and get early results for
research questions, such as “has the covariance structure between two
samples significantly changed?”.

## Features

For now we offer tools for: - Sampling data - Inspecting the spectra -
Hypothesis testing on the spectra

# Installation

We’ve submitted to CRAN but are not listed as of yet. In the mean time
you will have to install it from GitHub

``` r
library(devtools)
devtools::install_github("https://github.com/Carsten134/rspsp")
```

# Usage Example

Say you want to inspect whether the covariane structure between two
lattice data samples differ. For this you can use our `test.spectral`.

``` r
# simulate 20 x 20 MA grid under H1
K0 <- MA_coef_col(.7)
K1 <- MA_coef_row(.7)
x <- gridMA(20, 20, K0)
y <- gridMA(20, 20, K1)

# apply test statistic
test.spectral(x, y, 100, .05)
```

please see the [Getting Started
Guide](https://carsten134.github.io/rspsp/articles/rspsp.html) for more
detail.
