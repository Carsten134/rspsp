# Plot a spectral density

Plot a spectral density

## Usage

``` r
# S3 method for class 'spectral_density'
plot(x, ...)
```

## Arguments

- x:

  spectral_density object

- ...:

  additional arguments passed to the plot

## Examples

``` r
f <- function(x, y) cos(x) + cos(y)
class(f) <- c("spectral_density", "grid_function")
if (interactive()) {
  plot(f)
}
```
