# Generate Coefficient Matrix for MA Process

Generate Coefficient Matrix for MA Process

## Usage

``` r
MA_coef_all(value, size = 3)
```

## Arguments

- value:

  value to be added surounding

- size:

  size of the matrix

## Examples

``` r
MA_coef_all(0.5)
#>      [,1] [,2] [,3]
#> [1,]  0.5  0.5  0.5
#> [2,]  0.5  1.0  0.5
#> [3,]  0.5  0.5  0.5
```
