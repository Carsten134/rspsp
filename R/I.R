#' @title 2D Periodogramm
#'
#' @description Computes 2D Periodogramm. Consider the discrete Fourier transform of the sample:
#' \deqn{J({\omega}) = \frac{1}{\sqrt{2\pi NM}} \sum_{s\in S} x(s)\,\exp\!\left(-i\langle s,{\omega}\rangle\right)}
#'
#' Then the 2D periodogram is given by:
#' \deqn{I(\omega_{kl}) = J(\omega_{kl})\,\overline{J(\omega_{kl})} = \big|J(\omega_{kl})\big|^2}
#'
#' @param x matrix with data from stationary spatial process
#'
#' @returns martix with results
#'
#' @export
#' @examples
#' x <- gridMA(25, 25, MA_coef_all(.7))
#' image(I(x))
#'
#'
I <- function(x){
  N <- nrow(x)
  M <- ncol(x)

  # apply scaling ((2*pi)^2 is already in the fft)
  res <- abs(stats::fft(x))^2/(N * M)

  # fft shift such that 0 is at the center and nyquist is at the borders
  return(res[
    c(
      ((N%/% 2) + 2):N,
      1:((N %/% 2) + 1)),
    c(
      ((M%/% 2) + 2):M,
      1:(M %/% 2 + 1))]
  )
}

#' @noRd
I_cross <- function(x, y){
  N <- nrow(x)
  M <- ncol(x)
  # apply the fft to both x and y
  res <- abs(stats::fft(x) * Conj(stats::fft(y)))/(N * M * sqrt(stats::var(as.vector(x)) * stats::var(as.vector(y))))

  # finally do the fft shift
  return(res[
    c(
      ((N%/% 2) + 2):N,
      1:((N %/% 2) + 1)),
    c(
      ((M%/% 2) + 2):M,
      1:(M %/% 2 + 1))]
  )
}

#' @noRd
I_1d <- function(x) {
  N <- length(x)
  res <- abs(stats::fft(x))^2 / N

  return(res[
    c(
      ((N%/% 2) + 2):N,
      1:((N %/% 2) + 1))
    ])
}
