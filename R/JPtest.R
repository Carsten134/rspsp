
#' @export
#'
#' @title Jentsch Pauly Test
#'
#' @description Compute resampling test for equality of spectral densities or other qualities of the spectral densities.
#' Can be applied to both vectors and matrices. For more information see DOI: 10.3150/13-BEJ584.
#'
#' @param x first sample. Can be numeric vector or matrix
#' @param y second sample. Can be numeric vector or matrix (but must in it's type coincide with x). If isotropy is tested, this is disregarded.
#' @param B Number of iterations for resampling (The more the better). Can be numeric but must be a whole number
#' @param alpha Level of significance. Must be numeric value in (0,1]
#' @param print_result Boolean whether to print the result or just return an object cotaining the result
#' @param hypothesis Can be one of `"equality"` or `"isotropy"`
#'
#' @examples
#' # simulating grid-data under H0
#' K0 <- MA_coef_all(.3)
#' x <- gridMA(25, 25, K0)
#' y <- gridMA(25, 25, K0)
#'
#' # applying the test with 100 iterations and significance 5%
#' JPtest(x, y, 100, .05)
#'
JPtest <- function(x, y, B, alpha, print_result = TRUE, hypothesis="equality") {
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

  if (! is.numeric(x)) {
    stop(paste("x should be numeric, instead got", typeof(x)))
  }

  if (!is.numeric(y) & ! is.null(y)){
    stop(paste("y can be either numeric or NULL. Got", typeof(y)))
  }

  # vector case with equality
  if (is.vector(x) & is.vector(y) & hypothesis == "equality") {
    phi_n_star_1d(x, y, B, alpha, print_result)
  }

  # matrix case with equality
  else if (is.matrix(x) & is.matrix(y) & hypothesis == "equality"){
    phi_n_star_fast(x, y, B, alpha, print_result = print_result)
  }

  # matrix case with isotropy
  else if (is.matrix(x) & hypothesis == "isotropy") {
    phi_n_star_iso(x, B, alpha, .2, 3, print_result)
  }
}
