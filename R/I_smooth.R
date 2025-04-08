#' @title Smoothed Periodogram
#'
#' @description Computes smoothed Periodogram
#'
#' @param I periodogram results
#' @param kernel kernel used to smooth
#' @param hr rowwise bandwidth
#' @param hc columnwise bandwidth
I_smooth <- function(I,hr = .2, hc = .2) {
  N <- nrow(I)
  M <- ncol(I)

  I_s <- function (omega_1, omega_2) {
    weights <- w_2d_bp(omega_1, omega_2, N, M, hr, hc)
    weighted <- sum(weights * I)

    normalized <- (1/(N*M*hr*hc))*weighted
    return(normalized)
  }
  class(I_s) <- c("I_smooth", "grid_function")
  I_s
}
# I_smooth <- function(I, kernel = tr_k, hr = .5, hc = .5) {
#   N <- nrow(I)
#   M <- ncol(I)
#
#   omega_M <- 2*pi*((-((M - 2) %/% 2)):(M %/% 2))/M
#   omega_N <- 2*pi*((-((N - 2) %/% 2)):(N %/% 2))/N
#   I_s <- function (omega_1, omega_2) {
#     K_N <- kernel(omega_N - omega_1, hc)
#     K_M <- kernel(omega_M - omega_2, hr)
#
#     weights <- K_N %*% t(K_M)
#     weighted <- sum(weights * I)
#
#     normalized <- (1/(N*M*hr*hc))*weighted
#     return(normalized)
#   }
#   class(I_s) <- c("I_smooth", "grid_function")
#   I_s
# }

#' @export
evaluate <- function(f) {
  UseMethod("evaluate")
}

#' @export
evaluate.grid_function <- function(I_s, N = 30, M = 30) {
  x <- seq(-pi+1e-5, pi-1e-5, length.out = N)
  y <- seq(-pi+1e-5, pi-1e-5, length.out = M)


  z <- matrix(0, N, M)
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z[i, j] <- I_s(x[i], y[j])
    }
  }
  return(list(x = x, y = y, z = z))
}

#' @title Plot an smoothed periodogramm
#'
#' @description
#' This function plots a smoothed periodogramm.
#'
#' @param I_s I_smooth function
#'
#' @export
#'
plot.I_smooth <- function(I_s, ...) {
  vals = evaluate(I_s)
  persp(vals$x,
        vals$y,
        vals$z,
        ticktype = "detailed",
        zlab = "",
        xlab = "",
        ylab = "",
        main = "plot of smoothed Periodogram")
}

