# Print generic for testResult S3 class

Print generic for testResult S3 class

## Usage

``` r
# S3 method for class 'testResult'
print(x, ...)
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
print(res)
#> Test Result for equality type 
#>  ----------- 
#> Tn: 1 
#> p_value: 0.5 
#> decision:  Accepted H0 
```
