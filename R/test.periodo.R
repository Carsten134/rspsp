#' @title Periodogram Test for Assessing Equality of Spectral Densities
#'
#' @description
#' \eqn{\varphi_n^p} is an adaption of the test \eqn{T_3} from the work by Scaccia and Martin (2005). The test compares individual deviations of the periodogram ordinates
#'  \eqn{I_x} and \eqn{I_y} from the total amplitude of \eqn{I_x + I_y}. We define \eqn{G(\omega_{kl})} as the comparison value:
#'  \deqn{G(\omega_{kl}) = \frac{I_x(\omega_{kl})-I_y(\omega_{kl})}{I_x(\omega_{kl}) + I_y(\omega_{kl})}}
#'  The test statistic \eqn{PT_3} is then given by:
#'  \deqn{PT_3 = \sqrt{12n}(\overline{|G|-1/2}) \overset d\longrightarrow \mathcal N(0,1)}
#'  Critical values are drawn from the standard normal distirbution.
#'
#' @param x numeric matrix of dims N, M with no NA values (representing the first lattice data sample)
#' @param y numeric matrix of dims N, M with no NA values (representing the second lattice data sample)
#' @param alpha numeric value in \eqn{(0,1]}
#'
#' @returns An object of type periodoTestResult
#' @export
#' @examples
#' set.seed(1)
#' K0 <- MA_coef_all(0.3)
#' x <- gridMA(8, 8, K0)
#' y <- gridMA(8, 8, K0)
#' test.periodo(x, y, 0.05)
test.periodo <- function(x, y, alpha) {
  # basic type checking and
  stopifnot(is.numeric(x) & is.numeric(y), is.numeric(alpha))

  stopifnot(dim(x) == dim(y))

  pt3(x, y, alpha)
}
