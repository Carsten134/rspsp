#' @title Calculate Periodogram (Unshifted)
#'
#' @keywords internal
#'
#' @noRd
periodogram_unshifted <- function(x) {
  # R's fft is unnormalized.
  # PyTorch: (real^2 + imag^2) / (n1*n2)
  # R equivalent: Mod(fft(x))^2 / length(x)
  N <- nrow(x)
  M <- ncol(x)
  F_val <- stats::fft(x)
  return((Mod(F_val)^2) / (N * M))
}



#' @title Apply \eqn{PT_3}
#'
#' @description
#' \eqn{PT_3} is a self-regulating periodogram test. This function applies this test.
#'
#' @noRd
pt3 <- function(x, y, alpha) {
  # assume both lattices are equally sized
  N <- nrow(x)
  M <- ncol(x)

  # 1. Calculate Periodograms
  I_xu <- periodogram_unshifted(x)
  I_yu <- periodogram_unshifted(y)

  # 2. Define Frequency Limits (Strict interior first and third quadrant)
  max_freq_N <- floor((N - 1) / 2)
  max_freq_M <- floor((M - 1) / 2)

  # 3. Extract Frequencies.
  rows <- 2:(max_freq_N + 1)
  cols <- 2:(max_freq_M + 1)
  if (M > 3) {
    cols <- c(cols, (max_freq_M + 3):M)
  }

  I_x <- I_xu[rows, cols]
  I_y <- I_yu[rows, cols]

  # 4. Calculate Ratio
  # Add small epsilon to avoid division by zero if necessary, though unlikely in float
  eps <- .Machine$double.eps
  ratio <- abs((I_x - I_y) / (I_x + I_y + eps))

  # 5. Calculate Statistic
  n_star <- length(ratio)

  # Statistic: sqrt(12 * n_star) * (mean - 0.5)
  pt3_val <- sqrt(12 * n_star) * (mean(ratio) - 0.5)

  # 6. P-value and Decision (Two-sided test assumption, though PT3 usually checks upper tail)
  # Based on your Python code: p_val = 1 - pnorm. This implies a one-sided test
  # checking if the ratio deviation is larger than expected.
  p_val <- 1 - stats::pnorm(pt3_val)
  decision <- as.numeric(p_val < alpha)

  new_periodoTestResult(pt3_val, p_val, decision, alpha, "equality")
}

#' @title Constructor for periodoTestResult object
#'
#' @param test_value float value representing the value of \eqn{PT_3}
#' @param p_value float for \eqn{p} value
#' @param decision binary numeric (0, 1) value for acceptance (0) or rejection (1)
#' @param alpha float between 0 and 1 for significance level
#' @param hypothesis type of hypothesis
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
#' @param object periodoTestResult Object
#' @param ... additional parameters (might be used in later versions)
#'
#' @export
summary.periodoTestResult <- function(object, ...) {
  x <- object
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
#' @param x periodoTestResult object
#' @param ... additional arguments (might become relevant later)
#'
#' @export
print.periodoTestResult <- function(x, ...) {
  cat("Periodogram Test for", x$hypothesis, "Hypothesis. \n")
  cat("\n\nResults \n-----------------------------------------\n")
  cat("Tn:", x$test_value, "\n")
  cat("p:", x$p_value, "\n")
  cat("Decision:", if (x$decision )"Rejected H0" else "Accepted H0", "\n")
}




