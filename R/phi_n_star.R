phi_n_star <- function(x, y, B, alpha, hr=.1, hc=.2) {
  Tn_val <- Tn(x, y, hr, hc)

  Tn_star_val <- numeric(B)
  T_val <- numeric(B)
  for (i in 1:B) {
    Tn_star_val[i] <- Tn_star(x, y, hr, hc)
  }
  # accounting for machine error when comparing values
  p_value <- 1 - sum(Tn_star_val < Tn_val - 1e-15) / B
  decision <- as.numeric(p_value < alpha)

  result <- new_testResult(Tn_val,
                           Tn_star_val,
                           decision,
                           p_value,
                           "equality",
                           B,
                           alpha,
                           hr, hc)

  result
}


phi_n_star_fast <- function(x, y, B, alpha, hr = .2, hc = .2){
  # useful information
  N <- nrow(x)
  M <- ncol(x)

  K_BP <- k_2d_bp(N, M, hr, hc)


  # compute periodogramm
  I_x = I(x)
  I_y = I(y)
  I_tilde <- .5 * (I_x + I_y)


  # compute Tn
  I_x_smooth <- EBImage::filter2(I_x - I_tilde, K_BP, "circular")
  I_y_smooth <- EBImage::filter2(I_y - I_tilde, K_BP, "circular")

  # do Riemann sum over squared difference
  Tn_val <- sum(I_x_smooth^2 + I_y_smooth^2)*((2*pi)^2/(N*M))

  Tn_star_val <- numeric(B)
  for (i in 1:B) {
    # generate permuation
    perm <- generate_random_mask(N, M)
    # non-symmetrical permutation for comparison
    # perm <- matrix(as.numeric(runif(N*M) > .5),
    #                nrow=N)
    perm_n <- matrix(as.numeric(perm != 1),
                     nrow = N)
    I_x_rand <- I_x*perm + I_y*perm_n
    I_y_rand <- I_x*perm_n + I_y*perm

    # capture difference
    I_x_smooth <- EBImage::filter2(I_x_rand - I_tilde, K_BP, "circular")
    I_y_smooth <- EBImage::filter2(I_y_rand - I_tilde, K_BP, "circular")

    # integrate difference
    Tn_star_val[i] <- sum(I_x_smooth^2 + I_y_smooth^2)*((2*pi)^2/(N*M))
  }
  # accounting for machine error when comparing values
  p_value <- 1 - sum(Tn_star_val < Tn_val - 1e-15) / B
  decision <- as.numeric(p_value < alpha)

  result <- new_testResult(Tn_val,
                           Tn_star_val,
                           decision,
                           p_value,
                           "equality",
                           B,
                           alpha,
                           hr,hc)

  result
}

phi_n_star_iso <- function(x, B, alpha, h1, h2) {
  # get dims
  dims <- dim(x)
  N <- dims[1]; M <- dims[2]

  # compute kernel weights and periodogram
  K_BP <- k_2d_bp(N, N, h1, h2)
  I_x <- I(x)

  # precompute groups and demean
  orbit_groups <- get_d4_groups(N, M)
  I_x_diff <- apply_grouped(I_x, orbit_groups, function(x) x - mean(x))

  Tn_diff <- EBImage::filter2(I_x_diff, K_BP, "circular")
  Tn_val <- sum(Tn_diff^2) *((2*pi)^2 / (N * M))

  # Computing Tn_star
  Tn_star_val <- numeric(B)
  for (i in 1:B) {
    I_x_diff_rand <- apply_grouped(I_x_diff, orbit_groups, sample)
    diffs <- EBImage::filter2(I_x_diff_rand, K_BP, "circular")
    Tn_star_val[i] <- sum(diffs^2) * ((2*pi)^2 / (N * M))
  }
  # accounting for machine error when comparing values
  p_value <- 1 - sum(Tn_star_val < Tn_val - 1e-15) / B
  decision <- as.numeric(p_value < alpha)

  result <- new_testResult(Tn_val,
                           Tn_star_val,
                           decision,
                           p_value,
                           "isotropy",
                           B,
                           alpha,
                           h1,h2)
  result
}

phi_n_star_1d <- function(x, y, B, alpha, h) {
  # first instantiate kernel, length and padding...
  N <- length(x)
  K <- k_1d_bp(N, h)
  padding <- length(K) %/% 2

  # Then compute the periodogramm (with padding)
  I_x <- add_zero_padding_1d(I_1d(x), padding)
  I_y <- add_zero_padding_1d(I_1d(y), padding)
  I_tilde <- .5 * (I_x + I_y)

  # next smooth the differences
  I_x_smooth <- convolve(I_x - I_tilde, K, type = "f")
  I_y_smooth <- convolve(I_y - I_tilde, K, type = "f")

  # next compute the unconditional L2 Teststat (Riemann estimation)
  Tn_val <- sum(I_x_smooth^2 + I_y_smooth^2) / N * 2 * pi

  # Randomization
  Tn_star_val <- numeric(B)
  # tracks how quantile estimation evolves

  for (i in 1:B) {
    # permutate estimations
    perm <- as.numeric(runif(N + 2* padding) > .5)
    perm_n <- as.numeric(perm == 0)

    I_x_rand <- I_x * perm + I_y * perm_n
    I_y_rand <- I_x * perm_n + I_y * perm

    I_x_smooth <- convolve(I_x_rand - I_tilde, K, type = "f")
    I_y_smooth <- convolve(I_y_rand - I_tilde, K, type = "f")

    Tn_star_val[i] <- sum(I_x_smooth^2 + I_y_smooth^2) / N * pi * 2
  }
  # accounting for machine error when comparing values
  p_value <- 1-sum(Tn_star_val < Tn_val - 1e-13 ) / B
  decision <- as.numeric(p_value < alpha)

  new_testResult(Tn_val,
                 Tn_star_val,
                 decision,
                 p_value,
                 "equality",
                 B,
                 alpha,
                 h, h)
}


#' @title \eqn{\varphi_n^*} for \eqn{p=1} and arbitray \eqn{q}.
#'
phi_n_star_mv <- function(X, B, alpha, h1=.14, h2=.14) {
  dims <- dim(X)
  stopifnot(length(dims) == 3)

  N <- dims[1]; M <- dims[2]; q <- dims[3]

  # computing periodograms and differences
  I_X <- array(0, dim=dims)
  for(i in 1:q) {
    I_X[,,i] <- I(X[,,i])
  }
  I_tilde <- apply(X, c(1, 2), mean)

  for (i in 1:q) {
    I_X[,,i] <- I_X[,,i] - I_tilde
  }


  # computing Tn
  Tn <- 0
  Kh <- k_2d_bp(N, M, h1, h2)
  s <- sqrt(h1 * h2)/(N * M)
  for(i in 1:q) {
    Tn <- Tn + sum(EBImage::filter2(I_X[,,i], Kh, "circular")^2)
  }
  Tn <- s * Tn

  # computing Tn_star
  Tn_star <- numeric(B)
  for(j in 1:B) {
    I_X_rand <- permute_periodogram(I_X)
    current_Tn_star <- 0
    for (i in 1:q) {
      current_Tn_star <- current_Tn_star + sum(EBImage::filter2(I_X_rand[,,i], Kh, "circular")^2)
    }
    Tn_star[j] <- s * current_Tn_star
  }
  # accounting for machine error when comparing values
  p_value <- 1 - sum(Tn_star < Tn - 1e-13 ) / B
  decision <- as.numeric(p_value < alpha)

  new_testResult(Tn,
                 Tn_star,
                 decision,
                 p_value,
                 "equality",
                 B,
                 alpha,
                 h1, h2)
}

phi_n_stnry <- function(x, B, alpha, h1=.13, h2=.13) {
  dims <- dim(x)
  N <- dims[1]; M <- dims[2]

  # devide into four equal segments
  row_brd <- N %/% 2
  col_brd <- M %/% 2
  X <- array(0, dim=c(row_brd, col_brd, 4))
  X[,,1] <- x[1:row_brd,1:col_brd]
  X[,,2] <- x[(row_brd+1):(2*row_brd), 1:col_brd]
  X[,,3] <- x[1:row_brd, (col_brd+1):(2*col_brd)]
  X[,,4] <- x[(row_brd+1):(2*row_brd), (col_brd+1):(2*col_brd)]

  # test for equality of spectral densities in each partition
  phi_n_star_mv(X, B, alpha, h1, h2)
}





