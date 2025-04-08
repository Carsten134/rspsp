riemann_approx <- function(f, N = 100, M = 100) {
  omega_M <- 2*pi*((-((M - 2) %/% 2)):(M %/% 2))/M
  omega_N <- 2*pi*((-((N - 2) %/% 2)):(N %/% 2))/N

  sum <- 0
  for (i in 1:N) {
    for (j in 1:M) {
      sum <- sum + f(omega_N[i], omega_M[j])
    }
  }
  return(sum * ((2*pi)^2/(N*M)))
}
