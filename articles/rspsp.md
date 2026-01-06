# \`rspsp\`: Spatial Spectral Analysis in R

`rspsp` contains tools for analyzing spatial lattice data. Its main
features include:

1.  Samplers for random data
2.  Diagnostic tools for assessing the spectrum
3.  Test statistics for modeling assumptions and comparing spectra

## Generating Random Data

One of the easiest ways of fetching practice data for our tool is to
generate new synthetic data. We offer two different spatial processes
and one correlated extension:

- [`gridMA()`](https://carsten134.github.io/rspsp/reference/gridMA.md)
- [`gridAR()`](https://carsten134.github.io/rspsp/reference/gridAR.md)
- [`mvgridMA()`](https://carsten134.github.io/rspsp/reference/mvgridMA.md)

To generate a sample, simply generate coefficients using one of our
utils
([`MA_coef_all()`](https://carsten134.github.io/rspsp/reference/MA_coef_all.md),
[`MA_coef_row()`](https://carsten134.github.io/rspsp/reference/MA_coef_row.md),
[`MA_coef_col()`](https://carsten134.github.io/rspsp/reference/MA_coef_col.md))
and then use the sampler. For details on the usage of other samplers you
can see Reference.:

``` r
# generate new coefficient matrix for 
K0 <- MA_coef_all(.5)

x <- gridMA(75, 75, K0)

image(x)
```

![](rspsp_files/figure-html/generation-of-data-1.png)

## Inspecting the Periodogram and Spectral Density

Before testing your sample you can inspect the spectral properties with
the [`I()`](https://carsten134.github.io/rspsp/reference/I.md) function:

``` r
image(I(x))
```

![](rspsp_files/figure-html/plot-I-1.png) To inspect the spectral
density estimate use
[`I_smooth()`](https://carsten134.github.io/rspsp/reference/I_smooth.md)
we also offer a `plot` generic for the returned object:

``` r
I_x_smooth <- I_smooth(I(x))

plot(I_x_smooth)
```

![](rspsp_files/figure-html/plot-I-smooth-1.png)

## Testing for Equal Spectra

Test for equal spectra by either using by one of
[`test.periodo()`](https://carsten134.github.io/rspsp/reference/test.periodo.md)
or
[`test.spectral()`](https://carsten134.github.io/rspsp/reference/test.spectral.md).

``` r
y <- gridMA(75, 75, K0)

# apply periodogram based test Scaccia and Marin (2005)
summary(test.periodo(x, y, .05))
#> Periodogram Test for equality Hypothesis. 
#> =========================================
#> alpha: 0.05 
#> 
#> 
#> Results 
#> -----------------------------------------
#> Tn: -1.444126 
#> p: 0.9256483 
#> Decision: Accepted H0
```

``` r
# apply randomized test after Jentsch and Pauly (2015)
res <- test.spectral(x, y, 300, .05)
summary(res)
#> Resampling Test for equality Hypothesis. 
#> =========================================
#> Resampling iterations: 300 
#> alpha: 0.05 
#> used kernel-bandwith: 
#>  h1: 0.05624503 h2: 0.05624503
#> 
#> Results 
#> -----------------------------------------
#> Tn: 27.18329 
#> p: 0.7666667 
#> Decision: Accepted H0
```

You can also visualize the distribution of the randomized samples
against the L_2 type test T_n (see `test_spectral()` for more detail):

``` r
plot(res)
```

![](rspsp_files/figure-html/plot-res-equality-1.png)

## Testing for Isotropy

[`test.spectral()`](https://carsten134.github.io/rspsp/reference/test.spectral.md)
is applicable to other hypothesis for modelling assumptions. If you want
to test for weak isotropy simply set `hypothesis="isotropy"`

``` r
# make sure to pass NULL for y
res <- test.spectral(x, NULL, 300, .05, hypothesis="isotropy")

summary(res)
#> Resampling Test for isotropy Hypothesis. 
#> =========================================
#> Resampling iterations: 300 
#> alpha: 0.05 
#> used kernel-bandwith: 
#>  h1: 0.05624503 h2: 0.05624503
#> 
#> Results 
#> -----------------------------------------
#> Tn: 18.77008 
#> p: 0.9266667 
#> Decision: Accepted H0
plot(res)
```

![](rspsp_files/figure-html/plot-isotropy-1.png) If we would now
consider a non-isotropic process:

``` r
x <- gridMA(50, 50, MA_coef_row(.7))
image(I(x))
```

![](rspsp_files/figure-html/plot-I-alt-1.png) …the test will reject:

``` r
res <- test.spectral(x, NULL, 300, .05, hypothesis="isotropy")
#> Warning in test.spectral(x, NULL, 300, 0.05, hypothesis = "isotropy"): Kernel
#> smoothed just one summand. Critical values broke, because Tn is invariant
#> against permutation.
summary(res)
#> Resampling Test for isotropy Hypothesis. 
#> =========================================
#> Resampling iterations: 300 
#> alpha: 0.05 
#> used kernel-bandwith: 
#>  h1: 0.07369985 h2: 0.07369985
#> 
#> Results 
#> -----------------------------------------
#> Tn: 15.71547 
#> p: 0 
#> Decision: Rejected H0

plot(res)
```

![](rspsp_files/figure-html/plot-res-iso-alt-1.png)

you can also test for weak stationarity using `hypothesis=stationary`

``` r
h <- length(x)^(-.2)
res <- test.spectral(x, NULL, 300, .05, hypothesis="stationary", h1=h, h2=h)

summary(res)
#> Resampling Test for stationary Hypothesis. 
#> =========================================
#> Resampling iterations: 300 
#> alpha: 0.05 
#> used kernel-bandwith: 
#>  h1: 0.2091279 h2: 0.2091279
#> 
#> Results 
#> -----------------------------------------
#> Tn: 1.491642 
#> p: 0.8066667 
#> Decision: Accepted H0
plot(res)
```

![](rspsp_files/figure-html/plot-res-ws-1.png)
