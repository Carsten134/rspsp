#' @title Covariance function of a grid MA process
#'
#' @description
#' Computes the covariance of a grid MA process: \eqn{Cov(X_{s+k, t+l}, X_{s,t})}
#'
#'
#' @param k integer offset on row
#' @param l integer offset on column
#' @param K Coefficient matrix of "lag" polynomial
#'
#' @return numeric: value of \eqn{C(k,l) = Cov(X_{s+k,t+l},X_{s,t})}
C <- function(k,l, K) {
  # assuming K is a square matrix
  n <- nrow(K)
  # compute overlap in weights
  # but only if there is an overlap
  # being a bit more generous here to avoid
  # making the code too complicated
  if (abs(k) + abs(l) >= 2*n) {
    return(0)
  }

  first <- matrix(0, nrow = n + abs(k), ncol = n + abs(l))
  second <- matrix(0, nrow = n + abs(k), ncol = n + abs(l))

  first[1:n, 1:n] <- K
  second[(abs(k)+1):(n+abs(k)), (abs(l)+1):(n+abs(l))] <- K

  return(sum(first * second))
}
