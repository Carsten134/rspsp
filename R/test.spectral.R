
#' @export
#'
#' @title Resampling Test for Spectral Densities
#'
#' @description Compute resampling test for equality, isotropy, and weak stationarity of spectral densities or other qualities of the spectral densities.
#' Can be applied to both vectors and matrices. For more information see DOI: 10.3150/13-BEJ584.
#'
#' @param x first sample. Can be numeric vector or matrix
#' @param y second sample. Can be numeric vector or matrix (but must in it's type coincide with x). If isotropy is tested, this is disregarded.
#' @param B Number of iterations for resampling (The more the better). Can be numeric but must be a whole number
#' @param alpha Level of significance. Must be numeric value in (0,1)
#' @param hypothesis Can be one of `"equality", "isotropy", "stationary`
#' @param h1 Kernelbandwidth along first axis defaults to \eqn{(NM)^{-1/3}} (based on our simulation study this turned out to be a good option, if the lattice it not too rectangular)
#' @param h2 Kernelbandwidth along second axis defaults to \eqn{(NM)^{-1/3}}
#'
#' @examples
#' # simulating grid-data under H0
#' K0 <- MA_coef_all(.3)
#' x <- gridMA(25, 25, K0)
#' y <- gridMA(25, 25, K0)
#'
#' # applying the test with 300 iterations and significance 5%
#' test.spectral(x, y, 300, .05)
#'
test.spectral <- function(x, y, B, alpha, hypothesis="equality", h1=length(x)^(-.3333), h2=length(x)^(-.3333)) {
  # checking constraints for B and alpha
  if (! is.numeric(B)) {
    stop(paste("B should be numeric, instead got", typeof(B)))
  }
  if (B - as.integer(B) != 0) {
    stop("B should not be a floating-point number")
  }

  if (!is.numeric(alpha)) {
    stop(paste("alpha should be numeric, instead got", typeof(alpha)))
  }

  if (alpha <= 0 | alpha > 1) {
    stop(paste("Value for significance alpha has invalid value:", alpha, ". Choose a value in (0,1]"))
  }

  # type checking x and y arguments
  if (! is.numeric(x)) {
    stop(paste("x should be numeric, instead got", typeof(x)))
  }

  if (!is.numeric(y) & ! is.null(y)){
    stop(paste("y can be either numeric or NULL. Got", typeof(y)))
  }

  # checking dims of x and y
  dims_x <- dim(x)
  dims_y <- dim(y)
  if (hypothesis == "equality" & ! all(dims_x == dims_y)) {
    stop(paste("Got unequal dimensions with equality hypothesis. For x: ", dims_x, "for y:", dims_y))
  }

  # checking NA
  if (hypothesis == "equality" & (any(is.na(x) | is.na(y)))) {
    stop("Got na values in either x or y")
  }

  # finally, check if kernel smoothes at least one other summand
  num_sum <- dims_x * c(h1, h2) / 2
  if (all(num_sum < 1)) {
    warning("Kernel smoothed just one summand. Critical values broke, because Tn is invariant against permutation.")
  }

  # vector case with equality
  if (is.vector(x) & is.vector(y) & hypothesis == "equality") {
    phi_n_star_1d(x, y, B, alpha, h1)
  }

  # matrix case with equality
  else if (is.matrix(x) & is.matrix(y) & hypothesis == "equality"){
    phi_n_star_fast(x, y, B, alpha, h1, h2)
  }

  # matrix case with isotropy
  else if (is.matrix(x) & hypothesis == "isotropy") {
    phi_n_star_iso(x, B, alpha, h1, h2)
  }
  # matrix case with isotropy
  else if (is.matrix(x) & hypothesis == "stationary") {
    phi_n_stnry(x, B, alpha, h1, h2)
  }

}
