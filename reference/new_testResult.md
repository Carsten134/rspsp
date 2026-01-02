# Constructor for testResult object

Constructor for testResult object

## Usage

``` r
new_testResult(Tn, Tn_star, decision, p_value, hypothesis, B, alpha, h1, h2)
```

## Arguments

- Tn:

  float value for \\T_n\\

- Tn_star:

  float values (as vector of length \\B\\) representing all randomized
  values of \\T_n^\*\\

- decision:

  binary numeric (0, 1) value for acceptance (0) or rejection (1)

- p_value:

  float for \\p\\ value

- hypothesis:

  string for type of hypothesis tested

- B:

  integer representing the number of resampling iterations

- alpha:

  float between 0 and 1 for significance level

- h1:

  Bandwidth chosen for smoothing along row axis

- h2:

  Bandwidth chosen for smoothing along column axis

## Examples

``` r
res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
res
#> Test Result for equality type 
#>  ----------- 
#> Tn: 1 
#> p_value: 0.5 
#> decision:  Accepted H0 
```
