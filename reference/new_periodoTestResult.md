# Constructor for periodoTestResult object

Constructor for periodoTestResult object

## Usage

``` r
new_periodoTestResult(test_value, p_value, decision, alpha, hypothesis)
```

## Arguments

- test_value:

  float value representing the value of \\PT_3\\

- p_value:

  float for \\p\\ value

- decision:

  binary numeric (0, 1) value for acceptance (0) or rejection (1)

- alpha:

  float between 0 and 1 for significance level

- hypothesis:

  type of hypothesis

## Examples

``` r
res <- new_periodoTestResult(1.2, 0.1, 0, 0.05, "equality")
res
#> Periodogram Test for equality Hypothesis. 
#> 
#> 
#> Results 
#> -----------------------------------------
#> Tn: 1.2 
#> p: 0.1 
#> Decision: Accepted H0 
```
