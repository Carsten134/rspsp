tr_k <- function(u, h = 1) ifelse(u/h <= 1 & u / h >= -1, (35 / 32) * (1-(u/h)^2)^3, 0)

# implement kernel as outputing the weights for
# smoothing a 2d periodogram
w_2d_bp <- function(omega_1, omega_2, N, M, hr = .5, hc = .5) {
  w <- (8*gamma(1))/(4*gamma(.5)^2*pi^2)

  omega_N <- (fourier_freq(N) - omega_1)*(1/hr)
  omega_M <- (fourier_freq(M) - omega_2)*(1/hc)

  # get ||theta||^2
  theta_2 <- omega_N^2 %*% t(rep(1, M))  + rep(1, N) %*% t(omega_M)^2

  # w*(1- ||theta||^2/pi^2)
  return(ifelse(theta_2 < pi^2, w*(1-theta_2/pi^2), 0))
}
