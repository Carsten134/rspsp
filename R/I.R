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
  res <- (1/(N * M))*abs(fft(x))^2

  # fft shift such that 0 is at the center and nyquist is at the borders
  return(res[
    c(
      ((N %/% 2)+1):N,
      1:(N %/% 2)),
    c(
      ((M %/% 2)+1):M,
      1:(M %/% 2))]
  )
}
