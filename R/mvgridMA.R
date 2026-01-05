#' @title Correlated MA process
#'
#' @description
#' In our simulationstudy we also aim to find out how between sample correlation affects nominal size and power of our tests.
#' This MA-process introduces correlation on two levels: Innovations and whole lattices.
#' To do this, we first simulate two correlated white noise lattices \eqn{\{\epsilon_i^{(1)}|i\in \mathbb Z^2\}} and \eqn{\{\epsilon_i^{(2)}|i\in \mathbb Z^2\}}
#' Where:
#' \deqn{\begin{pmatrix}\epsilon_i^{(1)}\\ \epsilon_i^{(2)}\end{pmatrix}\overset{iid}\sim\mathcal N(0, \Sigma)}
#' With \eqn{\Sigma = \begin{pmatrix}1 & \sigma\\ \sigma & 1 \end{pmatrix}}
#'
#' The two univariate moving average randomfieds simulated by this function are not only convoluted with their own white noise (with \eqn{K_0, K_1})
#' ,but also their correlated counterparts (with \eqn{K_{0_{off}}, K_{1_{off}}}):
#' \deqn{X_i^{(1)} = \sum_{j}\epsilon_{i-j}^{(1)}K_{0,j} + \sum_l \epsilon_{i-l}^{(2)}K_{0_{off}, l}}
#' And analogous for \eqn{X_i^{(2)}}.
#'
#' @param N `integer` First dimension of lattices
#' @param M `integer` Second dimension of lattices
#' @param K0 \eqn{K_0}, numeric `matrix` used for \eqn{X_i^{(1)}} to convolute with own white noise lattice
#' @param K1 \eqn{K_1}, numeric `matrix` used for \eqn{X_i^{(2)}} to convolute with own white noise lattice
#' @param K0_off \eqn{K_{0_{off}}}, numeric `matrix` used for \eqn{X_i^{(1)}} to convolute with correlated counterpart
#' @param K1_off \eqn{K_{1_{off}}}, numeric `matrix` used for \eqn{X_i^{(2)}} to convolute with correlated counterpart
#' @param sigma \eqn{\sigma}, `numeric` for correlation between whitenoise
#'
#' @returns `list` object with entries X1 and X2
#'
#' @export
#' @examples
#' K <- MA_coef_all(.7)
#' K_off <- K * .5
#'
#' x <- mvgridMA(25, 25, K, K, K_off, K_off, .4)
#' par(mfrow=c(1,2))
#' image(x$X1)
#' image(x$X2)
#'
#'
mvgridMA <- function(N, M, K0, K1, K0_off, K1_off, sigma) {
  # building covariance matrix
  Sigma <- matrix(sigma, 2, 2)
  diag(Sigma) <- 1

  # constructing padding
  padding <- max(ncol(K0), ncol(K1)) %/% 2
  N_tilde <- N + 2 * padding
  M_tilde <- M + 2 * padding

  # generating data
  data <- mvrnorm_grid(N_tilde, M_tilde, Sigma)
  eps_x <- data$x
  eps_y <- data$y

  # convolution of Kernel on the grids
  X1 <- ravetools::convolve_image(eps_x, K0)
  X2 <- ravetools::convolve_image(eps_y, K1)
  X1_off <- ravetools::convolve_image(eps_y, K0_off)
  X2_off <- ravetools::convolve_image(eps_x, K1_off)
  X1 <- X1 + X1_off
  X2 <- X2 + X2_off
  # cut padding
  return(list(X1 = X1[(padding+1):(M+padding),(padding+1):(N+padding)],
              X2 = X2[(padding+1):(M+padding),(padding+1):(N+padding)]))
}
