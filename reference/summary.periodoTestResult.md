# summary generic for S3 testResult class

summary generic for S3 testResult class

## Usage

``` r
# S3 method for class 'periodoTestResult'
summary(object, ...)
```

## Arguments

- object:

  periodoTestResult Object

- ...:

  additional parameters (might be used in later versions)

## Examples

``` r
res <- new_periodoTestResult(1.2, 0.1, 0, 0.05, "equality")
summary(res)
#> Periodogram Test for equality Hypothesis. 
#> =========================================
#> alpha: 0.05 
#> 
#> 
#> Results 
#> -----------------------------------------
#> Tn: 1.2 
#> p: 0.1 
#> Decision: Accepted H0 
```
