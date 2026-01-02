# Generate Coefficient Matrix for MA Process

Generate Coefficient Matrix for MA Process

## Usage

``` r
MA_coef_col(value, size = 3)
```

## Arguments

- value:

  value to be added in column

- size:

  size of the matrix

## Examples

``` r
MA_coef_col(0.5)
#>      [,1] [,2] [,3]
#> [1,]    0  0.5    0
#> [2,]    0  1.0    0
#> [3,]    0  0.5    0
```
