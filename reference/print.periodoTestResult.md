# summary generic for S3 testResult class

summary generic for S3 testResult class

## Usage

``` r
# S3 method for class 'periodoTestResult'
print(x, ...)
```

## Arguments

- x:

  periodoTestResult object

- ...:

  additional arguments (might become relevant later)

## Examples

``` r
res <- new_periodoTestResult(1.2, 0.1, 0, 0.05, "equality")
print(res)
#> Periodogram Test for equality Hypothesis. 
#> 
#> 
#> Results 
#> -----------------------------------------
#> Tn: 1.2 
#> p: 0.1 
#> Decision: Accepted H0 
```
