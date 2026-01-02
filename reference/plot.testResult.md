# Print generic for testResult S3 class

Plots a histogram of \\T_n^\*\\ with \\T_n\\ marked as a blue vertical
line and \\c\_\alpha(T_n^\*)\\ (the critical value) marked as a red
vertical line

## Usage

``` r
# S3 method for class 'testResult'
plot(x, ...)
```

## Arguments

- x:

  testResult object

- ...:

  additional parameters for printing (might become relevant in future
  versions)

## Examples

``` r
res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
if (interactive()) {
  plot(res)
}
```
