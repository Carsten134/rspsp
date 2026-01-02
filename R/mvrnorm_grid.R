mvrnorm_grid <- function(N, M, Sigma) {
  # saving dependencies here and implementing my own
  # mvnorm function
  ev <- eigen(Sigma)
  L <- diag(ev$values)
  U <- ev$vectors

  # Solving A such that AA' = Sigma
  A <- U %*% sqrt(L) %*% t(U)

  # simulating the data
  x <- matrix(stats::rnorm(2 * N * M), nrow = 2)
  x <- A %*% x

  return(list(x = matrix(x[1,], N, M),
              y = matrix(x[2,], N, M)))
}
