# summary generic for S3 testResult class

summary generic for S3 testResult class

## Usage

``` r
# S3 method for class 'testResult'
summary(object, ...)
```

## Arguments

- object:

  testResult object

- ...:

  additional parameters for printing (might become relevant in future
  versions)

## Examples

``` r
res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
summary(res)
#> Resampling Test for equality Hypothesis. 
#> =========================================
#> Resampling iterations: 2 
#> alpha: 0.05 
#> used kernel-bandwith: 
#>  h1: 0.1 h2: 0.1
#> 
#> Results 
#> -----------------------------------------
#> Tn: 1 
#> p: 0.5 
#> Decision: Accepted H0 
```
