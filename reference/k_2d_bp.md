# Construct Kernel from Barlett-Priestley window

Used in the fast version of phi_n_star to construct a kernel for the
convolution

## Usage

``` r
k_2d_bp(N, M, hr = 0.5, hc = 0.5)
```

## Arguments

- N:

  positive integer for number of rows of lattice

- M:

  positive integer for number of cols of lattice

- hr:

  positive float for bandwidth along row axis

- hc:

  positive float for bandwidht along column axis
