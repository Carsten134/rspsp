riemann_approx <- function(f, N = 100, M = 100) {
  omega_M <- fourier_freq(M)
  omega_N <- fourier_freq(N)

  sum <- 0
  for (i in 1:N) {
    for (j in 1:M) {
      sum <- sum + f(omega_N[i], omega_M[j])
    }
  }
  return(sum * ((2*pi)^2/(N*M)))
}
