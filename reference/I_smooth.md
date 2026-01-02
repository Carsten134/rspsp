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
x <- matrix(rnorm(9), nrow = 3)
smoothed_periodo <- I_smooth(I(x))
plot(smoothed_periodo)

```
