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

fourier_dist <- function(N) {
  o_N <- fourier_freq(N)
  return(sqrt(o_N^2 %*% t(rep(1, N)) + rep(1, N) %*% t(o_N^2)))
}

#' @title Permutation in Anuli
shuffle_iso_part <- function(I_x) {
  # assume I_x is quadratic
  N <- ncol(I_x)
  o_N <- fourier_freq(N)

  # compute distance matrix
  dist <- sqrt(o_N^2 %*% t(rep(1, N)) + rep(1, N) %*% t(o_N^2))

  for (i in (N %/% 2 + N %% 2 + 2):N) {
    # get analus
    an_r <- dist <= o_N[i] & dist > o_N[i-1]

    # shuffle
    to_shuffle <- I_x[an_r]
    I_x[an_r] <- sample(to_shuffle, length(to_shuffle))
  }
  return(I_x)
}

#' @title Demeaning with exact euclidean distance
#' @export
demean_iso_exact <- function(I_x) {
  # assume I_x is quadratic
  M <- ncol(I_x)
  N <- nrow(I_x)
  o_N <- fourier_freq(N)
  o_M <- fourier_freq(M)

  # compute distance matrix
  dist <- sqrt(o_N^2 %*% t(rep(1, M)) + rep(1, N) %*% t(o_M^2))

  for (d in unique(as.vector(dist))) {
    # get analus
    an_r <- dist == d

    # demean
    I_x[an_r] <- I_x[an_r] - mean(I_x[an_r])
  }
  return(I_x)
}

#' @title Shuffeling with exact euclidean distance
shuffle_iso_exact <- function(I_x) {
  # assume I_x is quadratic
  M <- ncol(I_x)
  N <- nrow(I_x)
  o_N <- fourier_freq(N)
  o_M <- fourier_freq(M)

  # compute distance matrix
  dist <- sqrt(o_N^2 %*% t(rep(1, M)) + rep(1, N) %*% t(o_M^2))

  for (d in unique(as.vector(dist))) {
    # get analus
    an_r <- dist == d

    # demean
    to_shuffle <- I_x[an_r]
    I_x[an_r] <- sample(to_shuffle, length(to_shuffle))
  }
  return(I_x)
}


#' @title Demeaning in Anuli
demean_iso_part <- function(I_x) {
  # assume I_x is quadratic
  N <- ncol(I_x)
  o_N <- fourier_freq(N)

  # compute distance matrix
  dist <- sqrt(o_N^2 %*% t(rep(1, N)) + rep(1, N) %*% t(o_N^2))

  for (i in (N %/% 2 + N %% 2 + 2):N) {
    # get analus
    an_r <- dist <= o_N[i] & dist > o_N[i-1]

    # demean
    I_x[an_r] <- I_x[an_r] - mean(I_x[an_r])
  }
  return(I_x)
}

eval_star <- function(f, q, N) {
  thetas <- fourier_freq(q)

  r <- fourier_freq(N)
  r <- r[r > 0]
  # evaluate along every line
  values <- matrix(.0, length(r), q)
  for (t in 1:length(thetas)) {
    omega <- sqrt(r^2 / (1+tan(thetas[t])^2))
    omega_tilde <- tan(thetas[t]) * omega
    for (i in 1:length(r)) {
      values[i, t] <- f(omega[i], omega_tilde[i])
    }
  }
  return(values)
}

add_zero_padding_1d <- function(x, padding) {
  result <- numeric(length(x) + 2* padding)
  result[(padding + 1):(padding + length(x))] <- x
  return(result)
}

# generates a random mask that fulfills the symmetry property
generate_random_mask <- function(N, M) {
  N_zero <- (N - 1) %/% 2 + 1
  M_zero <- (M - 1) %/% 2 + 1

  perm <- matrix(as.numeric(runif(N * (M %/% 2)) > 0.5),
                 nrow = N, ncol = M %/% 2)

  mask <- matrix(0, nrow = N, ncol = M)

  # write perm into all rows, right-half columns
  if ((M_zero + 1) <= M && (M %/% 2) > 0) {
    mask[1:N, (M_zero + 1):M] <- perm
  }

  # 4 <- 1
  mask[(N_zero + 1):(N - (N - 1) %% 2), (M_zero - 1):1] <-
    mask[(N_zero - 1):1, (M_zero + 1):(M - (M - 1) %% 2)]

  # 3 <- 2
  mask[(N_zero - 1):1, (M_zero - 1):1] <-
    mask[(N_zero + 1):(N - (N - 1) %% 2), (M_zero + 1):(M - (M - 1) %% 2)]

  mask
}
