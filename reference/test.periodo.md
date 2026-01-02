# Periodogram Test for Assessing Equality of Spectral Densities

\\\varphi_n^p\\ is an adaption of the test \\T_3\\ from the work by
Scaccia and Martin (2005). The test compares individual deviations of
the periodogram ordinates \\I_x\\ and \\I_y\\ from the total amplitude
of \\I_x + I_y\\. We define \\G(\omega\_{kl})\\ as the comparison value:
\$\$G(\omega\_{kl}) =
\frac{I_x(\omega\_{kl})-I_y(\omega\_{kl})}{I_x(\omega\_{kl}) +
I_y(\omega\_{kl})}\$\$ The test statistic \\PT_3\\ is then given by:
\$\$PT_3 = \sqrt{12n}(\overline{\|G\|-1/2}) \overset d\longrightarrow
\mathcal N(0,1)\$\$ Critical values are drawn from the standard normal
distirbution.

## Usage

``` r
test.periodo(x, y, alpha)
```

## Arguments

- x:

  numeric matrix of dims N, M with no NA values (representing the first
  lattice data sample)

- y:

  numeric matrix of dims N, M with no NA values (representing the second
  lattice data sample)

- alpha:

  numeric value in \\(0,1\]\\

## Value

An object of type periodoTestResult

## Examples

``` r
set.seed(1)
K0 <- MA_coef_all(0.3)
x <- gridMA(8, 8, K0)
y <- gridMA(8, 8, K0)
test.periodo(x, y, 0.05)
#> Periodogram Test for equality Hypothesis. 
#> 
#> 
#> Results 
#> -----------------------------------------
#> Tn: -0.06621311 
#> p: 0.5263959 
#> Decision: Accepted H0 
```
