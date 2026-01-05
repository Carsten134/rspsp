#' @import ravetools
#' @export
#' @title Simulate a spatial MA process
#' @description
#' To validate the implementation we need data generated under both \eqn{H_0}
#' and the alternative. We therefore simulate a simple spatial \eqn{MA(q)} process.
#' The idea is to interpret lag coefficients as distance based influence on
#' \eqn{X_{ij}}, which can be implemented via a 2D convolution. The resulting
#' process is stationary with an isotropic covariance function.
#'
#' @param N rows
#' @param M columns of grid
#' @param K numeric matrix of kernel weights
#' @param distribution Type of distribution must be one of "normal", "uniform", "cauchy", "chisq"
#'
#' @return matrix with N rows and M columns
#' @examples
#' set.seed(1)
#' K <- MA_coef_all(0.3)
#' x <- gridMA(25, 25, K)
#' image(x)
gridMA <- function(N, M, K, distribution="normal") {
  padding <- nrow(K) %/% 2

  N_tilde <- N + 2 * padding
  M_tilde <- M + 2 * padding

  n <- N_tilde * M_tilde

  eps_val <- switch(distribution,
                    normal=stats::rnorm(n),
                    uniform=stats::runif(n),
                    cauchy=stats::rcauchy(n),
                    chisq=stats::rchisq(n, 3))

  eps <- matrix(eps_val,
                nrow = M_tilde,
                ncol = N_tilde)

  x <- ravetools::convolve_image(eps, K)
  # cut padding
  return(x[(padding+1):(M+padding),(padding+1):(N+padding)])
}
