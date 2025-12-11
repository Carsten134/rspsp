#' @title Calculate Periodogram (Unshifted)
periodogram_unshifted <- function(x) {
  # R's fft is unnormalized.
  # PyTorch: (real^2 + imag^2) / (n1*n2)
  # R equivalent: Mod(fft(x))^2 / length(x)
  N <- nrow(x)
  M <- ncol(x)
  F_val <- fft(x)
  return((Mod(F_val)^2) / (N * M))
}



#' @title Apply \eqn{PT_3}
#'
#' @description
#' \eqn{PT_3} is a self-regulating periodogram test. This function applies this test.
#'
pt3 <- function(x, y, alpha) {
  # assume both lattices are equally sized
  # N <- nrow(x)
  # M <- ncol(x)
  #
  # I_x <- I(x)[1:((N-1) %/% 2),1:((M-1) %/% 2)]
  # I_y <- I(y)[1:((N-1) %/% 2),1:((M-1) %/% 2)]
  #
  # n <- length(as.vector(I_x))
  # pt3_val <- sqrt(12 * n) * (mean(abs((I_x - I_y)/(I_x + I_y))) - .5)
  # p_val <- 1 - pnorm(pt3_val)
  # decision <- as.numeric(p_val < alpha)
  N <- nrow(x)
  M <- ncol(x)

  # 1. Calculate Periodograms
  I_xu <- periodogram_unshifted(x)
  I_yu <- periodogram_unshifted(y)

  # 2. Define Frequency Limits (Strict interior first quadrant)
  # Matches Python: (n-1) // 2
  max_freq_N <- floor((N - 1) / 2)
  max_freq_M <- floor((M - 1) / 2)

  # 3. Extract Frequencies
  # Python starts at index 1 (skipping 0).
  # R starts at index 1 (DC). So we must use index 2 to skip DC.
  # Python range is inclusive of max, so we go up to max_freq + 1 in R indices.
  rows <- 2:(max_freq_N + 1)
  cols <- 2:(max_freq_M + 1)

  I_x <- I_xu[rows,]
  I_y <- I_yu[rows,]

  # 4. Calculate Ratio
  # Add small epsilon to avoid division by zero if necessary, though unlikely in float
  eps <- .Machine$double.eps
  ratio <- abs((I_x - I_y) / (I_x + I_y + eps))

  # 5. Calculate Statistic
  # CRITICAL FIX: Use the number of elements in the subset, not total pixels
  n_star <- length(ratio)

  # Statistic: sqrt(12 * n_star) * (mean - 0.5)
  pt3_val <- sqrt(12 * n_star) * (mean(ratio) - 0.5)

  # 6. P-value and Decision (Two-sided test assumption, though PT3 usually checks upper tail)
  # Based on your Python code: p_val = 1 - pnorm. This implies a one-sided test
  # checking if the ratio deviation is larger than expected.
  p_val <- 1 - pnorm(pt3_val)
  decision <- as.numeric(p_val < alpha)

  new_periodoTestResult(pt3_val, p_val, decision, alpha, "equality")
}

#' @title Constructor for periodoTestResult object
#'
#' @export
new_periodoTestResult <- function(test_value, p_value, decision, alpha, hypothesis) {
  stopifnot(is.numeric(test_value))
  stopifnot(is.numeric(decision))
  stopifnot(is.numeric(p_value) | p_value < 0 | p_value > 1)
  stopifnot(is.character(hypothesis))
  stopifnot(is.numeric(alpha) | alpha < 0 | alpha > 1)

  structure(list(
    test_value=test_value,
    decision=decision,
    p_value=p_value,
    hypothesis=hypothesis,
    alpha=alpha
  ),
  class="periodoTestResult")
}


#' @title summary generic for S3 testResult class
#'
#' @export
summary.periodoTestResult <- function(x, ...) {
  cat("Periodogram Test for", x$hypothesis, "Hypothesis. \n")
  cat("=========================================\n")
  cat("alpha:", x$alpha, "\n")
  cat("\n\nResults \n-----------------------------------------\n")
  cat("Tn:", x$test_value, "\n")
  cat("p:", x$p_value, "\n")
  cat("Decision:", if (x$decision )"Rejected H0" else "Accepted H0", "\n")
}

#' @title summary generic for S3 testResult class
#'
#' @export
print.periodoTestResult <- function(x, ...) {
  cat("Periodogram Test for", x$hypothesis, "Hypothesis. \n")
  cat("\n\nResults \n-----------------------------------------\n")
  cat("Tn:", x$test_value, "\n")
  cat("p:", x$p_value, "\n")
  cat("Decision:", if (x$decision )"Rejected H0" else "Accepted H0", "\n")
}




