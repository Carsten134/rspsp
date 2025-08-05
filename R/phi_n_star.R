phi_n_star <- function(x, y, B, alpha, print_result = T, ...) {
  Tn_val <- Tn(x, y,...)

  Tn_star_val <- numeric(B)
  T_val <- numeric(B)
  for (i in 1:B) {
    Tn_star_val[i] <- Tn_star(x, y,...)
    T_val[i] <- sum(Tn_star_val[1:i] < Tn_val)/i
  }

  decision <- T_val[B] > 1-alpha

  result <- list(decision = as.numeric(decision),
                 Tn = Tn_val,
                 Tn_star = Tn_star_val,
                 T_val = T_val)

  if (print_result == T) {
    print.phi_n_star(result, alpha, B)
  } else {
    return(result)
  }
}


phi_n_star_fast <- function(x, y, B, alpha, hr = .2, hc = .2, print_result = T){
  # useful information
  N <- nrow(x)
  M <- ncol(x)

  K_BP <- k_2d_bp(N, M, hr, hc)
  padding_M <- ncol(K_BP) %/% 2
  padding_N <- nrow(K_BP) %/% 2

  # compute periodogramm (with padding)
  I_x <- add_zero_padding(I(x), padding_N, padding_M)
  I_y <- add_zero_padding(I(y), padding_N, padding_M)
  I_tilde <- .5 * (I_x + I_y)


  # compute Tn
  I_x_smooth <- ravetools::convolve_image(I_x - I_tilde, K_BP)
  I_y_smooth <- ravetools::convolve_image(I_y - I_tilde, K_BP)

  # do Riemann sum over squared difference
  Tn_val <- sum(I_x_smooth^2 + I_y_smooth^2)*((2*pi)^2/(N*M))

  Tn_star_val <- numeric(B)
  T_val <- numeric(B)
  for (i in 1:B) {
    # generate permuation
    perm <- generate_random_mask(N, M)
    perm_n <- matrix(as.numeric(perm != 1),
                     nrow = N + 2 * padding_N)
    I_x_rand <- I_x*perm + I_y*perm_n
    I_y_rand <- I_x*perm_n + I_y*perm

    # capture difference
    I_x_smooth <- ravetools::convolve_image(I_x_rand - I_tilde, K_BP)
    I_y_smooth <- ravetools::convolve_image(I_y_rand - I_tilde, K_BP)

    # integrate difference
    Tn_star_val[i] <- sum(I_x_smooth^2 + I_y_smooth^2)*((2*pi)^2/(N*M))
    T_val[i] <- sum(Tn_star_val[1:i] < Tn_val)/i
  }

  decision <- as.numeric(T_val[B] > 1-alpha)

  result <- list(decision = as.numeric(decision),
                 Tn = Tn_val,
                 Tn_star = Tn_star_val,
                 T_val = T_val)

  if (print_result == T) {
    print.phi_n_star(result, alpha, B)
  } else {
    return(result)
  }
}

phi_n_star_iso <- function(x, B, alpha, h, q, print_result = T) {
  # assume square x
  N <- nrow(x)
  # Computing Tn
  I_x_diff <- demean_iso(I(x))
  Tn_diff <- eval_star(I_smooth(I_x_diff, h,h), q, N)
  Tn_val <- sum(Tn_diff^2) *(2*pi / N)

  # Computing Tn_star
  Tn_star_val <- numeric(B)
  T_val <- numeric(B)
  for (i in 1:B) {
    I_x_diff_rand <- shuffle_circ(I_x_diff)
    diffs <- eval_star(I_smooth(I_x_diff_rand, h, h), q, N)
    Tn_star_val[i] <- sum(diffs^2) * (2*pi/N)
    T_val[i] <- sum(Tn_star_val[1:i] < Tn_val)/ i
  }

  decision <- as.numeric(T_val[B] > 1-alpha)

  result <- list(decision = as.numeric(decision),
                 Tn = Tn_val,
                 Tn_star = Tn_star_val,
                 T_val = T_val)

  if(print_result==True) {
    print.phi_n_star(result, alpha, B)
  } else {
    return(result)
  }

}

phi_n_star_1d <- function(x, y, B, alpha, h, print_result = T) {
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
  T_val <- numeric(B)

  for (i in 1:B) {
    # permutate estimations
    perm <- as.numeric(runif(N + 2* padding) > .5)
    perm_n <- as.numeric(perm == 0)

    I_x_rand <- I_x * perm + I_y * perm_n
    I_y_rand <- I_x * perm_n + I_y * perm

    I_x_smooth <- convolve(I_x_rand - I_tilde, K, type = "f")
    I_y_smooth <- convolve(I_y_rand - I_tilde, K, type = "f")

    Tn_star_val[i] <- sum(I_x_smooth^2 + I_y_smooth^2) / N * pi * 2
    T_val[i] <- sum(Tn_star_val[1:i] < Tn_val)/i
  }

  decision <- as.numeric(T_val[B] > 1- alpha)

  result <- list(decision = as.numeric(decision),
                 Tn = Tn_val,
                 Tn_star = Tn_star_val,
                 T_val = T_val)

  if (print_result == T) {
    print.phi_n_star(result, alpha, B)
  } else {
    return(result)
  }
}

#' @export
print.phi_n_star <- function(result, alpha, B) {
  Tn_star_val <- result$Tn_star
  decision <- result$decision
  Tn_val <- result$Tn
  T_val <- result$T_val

  par(mfrow = c(1,2))

  hist(Tn_star_val, main = "Histogram of Tn*",
       xlim = c(min(c(Tn_star_val, Tn_val)), max(c(Tn_star_val, Tn_val))),
       breaks = 30)
  abline(v = Tn_val, col = "red")
  plot(T_val, main = "Trace of T over all randomizations",
       xlab = "b",
       ylab = "T",
       ylim = c(0,1),
       type = "l")
  abline(h = 1-alpha, col = "red")

  print("Test for equality of spatial ACF and spectral density")
  print(paste("Randomized samples: ", B))
  print(paste("Tn: ", Tn_val))
  print(paste("Percentage of Tn > Tn*: ", T_val[B]*100, "%"))
  if (decision) {
    print("Decision: Rejecting H0")
  } else {
    print("Decision: Accepting H0")
  }
}


