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

#' @title Pre-compute D4 Orbit Groups
#' @description Maps every pixel to its D4 symmetry orbit
get_d4_groups <- function(N, M) {
  # Generate centered coordinate vectors
  u_vec <- (1:N) - (floor(N / 2) + 1)
  v_vec <- (1:M) - (floor(M / 2) + 1)

  U <- matrix(rep(u_vec, M), nrow = N, ncol = M)
  V <- matrix(rep(v_vec, each = N), nrow = N, ncol = M)

  # find canonical representatives
  abs_u <- abs(U)
  abs_v <- abs(V)
  p1 <- pmax(abs_u, abs_v)
  p2 <- pmin(abs_u, abs_v)

  # generate ids
  orbit_id <- p1 * (max(N, M) + 1) + p2

  # Split linear indices into groups by Orbit ID
  return(split(1:(N * M), orbit_id))
}

#' @title Functional for precomputed D4 Orbit Groups
#' @param spec_mat N x M matrix
#' @param groups List of indices (orbits)
#' @param fun A function that takes a vector and returns a vector of the same length
#' @param ... Additional arguments passed to fun
apply_grouped <- function(spec_mat, groups, fun, ...) {
  # Apply function to values within each orbit
  processed_list <- lapply(groups, function(idx) {
    fun(spec_mat[idx], ...)
  })

  # Flatten indices and values for vectorized assignment
  target_indices <- unlist(groups, use.names = FALSE)
  processed_values <- unlist(processed_list, use.names = FALSE)

  out_mat <- spec_mat
  out_mat[target_indices] <- processed_values

  return(out_mat)
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


permute_periodogram <- function(arr) {
  dims <- dim(arr)
  N <- dims[1]
  M <- dims[2]
  q <- dims[3]
  P <- N * M

  get_sym_indices <- function(K) {
    if (K %% 2 != 0) {
      # Odd Case: Pure reversal (e.g., 1 2 3 4 5 -> 5 4 3 2 1)
      return((K + 1) - (1:K))
    } else {
      # Even Case: Reverse K-1, keep K fixed (e.g., 1 2 3 4 -> 3 2 1 4)
      idx <- K - (1:K)
      idx[K] <- K # Fix the Nyquist frequency (last element)
      return(idx)
    }
  }

  rows_sym_map <- get_sym_indices(N)
  cols_sym_map <- get_sym_indices(M)

  r_vec <- rep(1:N, times = M)
  c_vec <- rep(1:M, each = N)

  r_sym <- rows_sym_map[r_vec]
  c_sym <- cols_sym_map[c_vec]

  # Convert symmetric (r, c) coordinates to linear indices
  idx_original <- 1:P
  idx_symmetric <- r_sym + (c_sym - 1) * N

  idx_leader <- pmin(idx_original, idx_symmetric)

  raw_perms <- t(replicate(P, sample.int(q)))

  sym_perms <- raw_perms[idx_leader, ]

  # Calculate final linear read offsets
  fetch_indices <- idx_original + (sym_perms - 1) * P

  # Extract and reshape
  result <- array(arr[as.vector(fetch_indices)], dim = dims)

  return(result)
}
