#' @title 2D Periodogramm
#'
#' @description Computes 2D Periodogramm
#'
#' @param x matrix with data from stationary spatial process
#'
#' @returns martix with results
I <- function(x){
  N <- nrow(x)
  M <- ncol(x)

  # apply scaling ((2*pi)^2 is already in the fft)
  # also devide by variance to make it a spectral density
  res <- (1/(N * M * var(as.vector(x))))*abs(fft(x))^2

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

#' @export
I_1d <- function(x) {
  N <- length(x)
  res <- abs(fft(x))^2/(N * sqrt(var(x)))

  return(res[
    c(
      ((N%/% 2) + 2):N,
      1:((N %/% 2) + 1))
    ])
}
