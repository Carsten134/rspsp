#' @title Smoothed Periodogram
#'
#' @description Computes smoothed Periodogram
#'
#' @param I periodogram results
#' @param hr rowwise bandwidth
#' @param hc columnwise bandwidth
#'
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

#' @title evaluate generic
#'
#' @description
#' Evaluates functions on a grid
#'
#' @param f function
#' @param N integer for rows of output matrix
#' @param M integer for cols of output matrix
#'
#' @export
evaluate <- function(f, N, M) {
  UseMethod("evaluate")
}


#' @title evaluate a grid function object
#'
#' @description
#' Evaluates a function on \eqn{[-\pi, \pi]^2}
#'
#'
#' @param f the grid_function object
#' @param N integer for number of rows to generate
#' @param M integer for number of columns to generate
#'
#'
evaluate.grid_function <- function(f, N = 30, M = 30) {
  x <- seq(-pi+1e-5, pi-1e-5, length.out = N)
  y <- seq(-pi+1e-5, pi-1e-5, length.out = M)


  z <- matrix(0, N, M)
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z[i, j] <- f(x[i], y[j])
    }
  }
  return(list(x = x, y = y, z = z))
}

#' @title Plot an smoothed periodogramm
#'
#' @description
#' This function plots a smoothed periodogramm.
#'
#' @param x I_smooth function
#' @param ... additional arguments might become relevant in later versions
#'
#' @export
#'
plot.I_smooth <- function(x, ...) {
  I_s <- x
  vals = evaluate(I_s)
  graphics::persp(vals$x,
                  vals$y,
                  vals$z,
                  ticktype = "detailed",
                  zlab = "",
                  xlab = "",
                  ylab = "",
                  main = "plot of smoothed Periodogram")
}

