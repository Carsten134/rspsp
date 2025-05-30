tr_k <- function(u, h = 1) ifelse(u/h <= 1 & u / h >= -1, (35 / 32) * (1-(u/h)^2)^3, 0)

# implement kernel as outputing the weights for
# smoothing a 2d periodogram
w_2d_bp <- function(omega_1, omega_2, N, M, hr = .5, hc = .5) {
  w <- (8*gamma(1))/(4*gamma(.5)^2*pi^2)

  omega_N <- (omega_1 - fourier_freq(N))*(1/hr)
  omega_M <- (omega_2 - fourier_freq(M))*(1/hc)

  # get ||theta||^2
  theta_2 <- omega_N^2 %*% t(rep(1, M))  + rep(1, N) %*% t(omega_M)^2

  # w*(1- ||theta||^2/pi^2)
  return(ifelse(theta_2 < pi^2, w*(1-theta_2/pi^2), 0))
}

#' @title Construct Kernel from Barlett-Priestley window
#'
#' @description Used in the fast version of phi_n_star to construct a kernel for the convolution
k_2d_bp <- function(N, M, hr = .5, hc = .5) {
  w <- (8*gamma(1))/(4*gamma(.5)^2*pi^2)

  omega_N <- (fourier_freq(N)*(1/hr))^2
  omega_M <- (fourier_freq(M)*(1/hc))^2


  omega_N <- omega_N[omega_N < pi^2]
  omega_M <- omega_M[omega_M < pi^2]
  # get ||theta||^2
  theta_2 <- omega_N %*% t(rep(1, length(omega_M)))  + rep(1, length(omega_N)) %*% t(omega_M)

  theta_2[theta_2 >= pi^2] <- pi^2

  return(w*(1-theta_2/pi^2))
}

k_1d_bp <- function(N, h=.1) {
  w <- .75 / (pi * h)

  omega <- (fourier_freq(N)/h)^2
  omega <- omega[omega < pi^2]

  return(w * (1-omega/pi^2))
}
