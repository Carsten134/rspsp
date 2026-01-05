# Smoothed Periodogram

Computes smoothed Periodogram

## Usage

``` r
I_smooth(I, hr = 0.2, hc = 0.2)
```

## Arguments

- I:

  periodogram results

- hr:

  rowwise bandwidth

- hc:

  columnwise bandwidth

## Examples

``` r
set.seed(1)
K <- MA_coef_all(.7)
x <- gridMA(25, 25, K)
smoothed_periodo <- I_smooth(I(x))
plot(smoothed_periodo)

```
