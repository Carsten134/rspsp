fourier_freq <- function(N) 2*pi*((-((N - 1) %/% 2)):(N %/% 2))/N

MA_coef_col <- function(value, size = 3) {
  K <- matrix(0, size, size)
  K[, (size %/% 2 + 1)] <- value
  K[(size %/% 2 + 1), (size %/% 2 + 1)] <- 1
  return(K)
}

MA_coef_row <- function(value, size = 3) {
  K <- matrix(0, size, size)
  K[(size %/% 2 + 1), ] <- value
  K[(size %/% 2 + 1), (size %/% 2 + 1)] <- 1
  return(K)
}

MA_coef_all <- function(value, size = 3) {
  K <- matrix(value, size, size)
  K[(size %/% 2 + 1), (size %/% 2 + 1)] <- 1
  return(K)
}

add_zero_padding <- function(A, padding_row, padding_col) {
  result <- matrix(0,
                   nrow(A) + 2 * padding_row,
                   ncol(A) + 2 * padding_col)
  result[(padding_row + 1):(padding_row + nrow(A)),
         (padding_col + 1):(padding_col + ncol(A))] <- A
  return(result)
}
