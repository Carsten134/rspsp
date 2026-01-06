# 2D Periodogramm

Computes 2D Periodogramm. Consider the discrete Fourier transform of the
sample: \$\$J({\omega}) = \frac{1}{\sqrt{2\pi NM}} \sum\_{s\in S}
x(s)\\\exp\\\left(-i\langle s,{\omega}\rangle\right)\$\$

Then the 2D periodogram is given by: \$\$I(\omega\_{kl}) =
J(\omega\_{kl})\\\overline{J(\omega\_{kl})} =
\big\|J(\omega\_{kl})\big\|^2\$\$

## Usage

``` r
I(x)
```

## Arguments

- x:

  matrix with data from stationary spatial process

## Value

martix with results
