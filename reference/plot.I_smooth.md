# Plot an smoothed periodogramm

This function plots a smoothed periodogramm.

## Usage

``` r
# S3 method for class 'I_smooth'
plot(x, ...)
```

## Arguments

- x:

  I_smooth function

- ...:

  additional arguments might become relevant in later versions

## Examples

``` r
set.seed(1)
I <- matrix(rnorm(9), nrow = 3)
spec <- I_smooth(I)
if (interactive()) {
  plot(spec)
}
```
