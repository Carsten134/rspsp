f_true_MA <- function(K) {
  K <- K
  n <- ncol(K)
  f <- function(omega, omega_tilde) {
    summation <- 0
    for (k in -n:n) {
      for (l in -n:n) {
        summation <- summation + C(k, l, K) * exp(-1i*(k * omega + l*omega_tilde))
      }
    }
    return(Re((1/(2*pi)^2)*summation))
  }
  class(f) <- c("spectral_density", "grid_function")
  return(f)
}

#' @exportS3Method base::plot
plot.spectral_density <- function(x, ...) {
  f <- x
  vals = evaluate(f)
  graphics::persp(vals$x, vals$y, vals$z,
                  xlab = "",
                  ylab = "",
                  zlab = "",
                  zlim = c(0, max(vals$z)),
                  ticktype = "detailed",
                  main = "plot of True spectral density")
}
