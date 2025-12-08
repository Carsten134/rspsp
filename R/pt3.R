#' @title Apply \eqn{PT_3}
#'
#' @description
#' \eqn{PT_3} is a self-regulating periodogram test. This function applies this test.
#'
pt3 <- function(x, y, alpha) {
  # assume both lattices are equally sized
  N <- nrow(x)
  M <- ncol(x)

  n <- N*M

  if (N > M) {
    I_x <- I(x)[1:((N-1) %/% 2 + 1),]
    I_y <- I(y)[1:((N-1) %/% 2 + 1),]
  } else {
    I_x <- I(x)[, 1:((M-1) %/% 2 + 1)]
    I_y <- I(y)[, 1:((M-1) %/% 2 + 1)]
  }

  pt3_val <- sqrt(12 * n) * (mean(abs((I_x - I_y)/(I_x + I_y))) - .5)
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




