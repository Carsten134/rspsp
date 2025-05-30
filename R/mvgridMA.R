mvgridMA <- function(N, M, K0, K1, sigma) {
  # building covariance matrix
  Sigma <- matrix(sigma, 2, 2)
  diag(Sigma) <- 1

  # constructing padding
  padding <- ncol(K) %/% 2
  N_tilde <- N + 2 * padding
  M_tilde <- M + 2 * padding

  # generating data
  data <- mvrnorm_grid(N_tilde, M_tilde, Sigma)
  eps_x <- data$x
  eps_y <- data$y

  # convolution of Kernel on the grids
  x <- ravetools::convolve_image(eps_x, K0)
  y <- ravetools::convolve_image(eps_y, K1)
  # cut padding
  return(list(x = x[(padding+1):(M+padding),(padding+1):(N+padding)],
              y = y[(padding+1):(M+padding),(padding+1):(N+padding)]))
}
