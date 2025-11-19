phi_n_star <- function(x, y, B, alpha, hr=.1, hc=.2) {
  Tn_val <- Tn(x, y, hr, hc)

  Tn_star_val <- numeric(B)
  T_val <- numeric(B)
  for (i in 1:B) {
    Tn_star_val[i] <- Tn_star(x, y, hr, hc)
  }
  # accounting for machine error when comparing values
  p_value <- sum(Tn_star_val < Tn_val - 1e-15) / B
  decision <- as.numeric(p_value > 1-alpha)

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
  p_value <- sum(Tn_star_val < Tn_val - 1e-15) / B
  decision <- as.numeric(p_value > 1-alpha)

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
  # assume square x
  N <- nrow(x)
  # Computing Tn
  I_x_diff <- demean_iso_exact(I(x))
  K_BP <- k_2d_bp(N, N, h1, h2)

  Tn_diff <- EBImage::filter2(I_x_diff, K_BP, "circular")
  Tn_val <- sum(Tn_diff^2) *((2*pi)^2 / N^2)

  # Computing Tn_star
  Tn_star_val <- numeric(B)
  for (i in 1:B) {
    I_x_diff_rand <- shuffle_iso_exact(I_x_diff)
    diffs <- EBImage::filter2(I_x_diff_rand, K_BP, "circular")
    Tn_star_val[i] <- sum(diffs^2) * ((2*pi)^2 / N^2)
  }
  # accounting for machine error when comparing values
  p_value <- sum(Tn_star_val < Tn_val - 1e-13) / B
  decision <- as.numeric(p_value > 1-alpha)

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
  p_value <- sum(Tn_star_val < Tn_val - 1e-13 ) / B
  decision <- as.numeric(p_value > 1- alpha)

  result <- new_testResult(Tn_val,
                           Tn_star_val,
                           decision,
                           p_value,
                           "equality",
                           B,
                           alpha,
                           h, h)
  result
}



