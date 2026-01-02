
#' @title Constructor for testResult object
#'
#' @param Tn float value for \eqn{T_n}
#' @param Tn_star float values (as vector of length \eqn{B}) representing all randomized values of \eqn{T_n^*}
#' @param decision binary numeric (0, 1) value for acceptance (0) or rejection (1)
#' @param p_value float for \eqn{p} value
#' @param hypothesis string for type of hypothesis tested
#' @param B integer representing the number of resampling iterations
#' @param alpha float between 0 and 1 for significance level
#' @param h1 Bandwidth chosen for smoothing along row axis
#' @param h2 Bandwidth chosen for smoothing along column axis
#'
#' @export
#' @examples
#' res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
#' res
new_testResult <- function(Tn, Tn_star, decision, p_value, hypothesis, B, alpha, h1, h2) {
  stopifnot(is.numeric(Tn))
  stopifnot(is.numeric(Tn_star))
  stopifnot(is.numeric(decision))
  stopifnot(is.numeric(p_value) | p_value < 0 | p_value > 1)
  stopifnot(is.character(hypothesis))
  stopifnot(is.numeric(B))
  stopifnot(is.numeric(alpha) | alpha < 0 | alpha > 1)
  stopifnot(is.numeric(h1))
  stopifnot(is.numeric(h2))

  structure(list(
    Tn=Tn,
    Tn_star=Tn_star,
    decision=decision,
    p_value=p_value,
    hypothesis=hypothesis,
    B=B,
    alpha=alpha,
    h1=h1,
    h2=h2
  ),
  class="testResult")
}

#' @title Print generic for testResult S3 class
#'
#' @param x testResult object
#' @param ... additional parameters for printing (might become relevant in future versions)
#'
#' @export
#' @examples
#' res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
#' print(res)
print.testResult <- function(x, ...) {
  cat("Test Result for", x$hypothesis, "type", "\n", "-----------", "\n")
  cat("Tn:", x$Tn, "\n")
  cat("p_value:", x$p_value, "\n")
  cat("decision: ", if (x$decision )"Rejected H0" else "Accepted H0", "\n")
}

#' @title Print generic for testResult S3 class
#'
#' @description
#' Plots a histogram of \eqn{T_n^*} with \eqn{T_n} marked as a blue vertical line
#' and \eqn{c_\alpha(T_n^*)} (the critical value) marked as a red vertical line
#'
#'
#' @param x testResult object
#' @param ... additional parameters for printing (might become relevant in future versions)
#'
#' @export
#' @examples
#' res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
#' if (interactive()) {
#'   plot(res)
#' }
plot.testResult <- function(x, ...) {
  Tn_star_val <- x$Tn_star
  decision <- x$decision
  Tn_val <- x$Tn
  alpha <- x$alpha
  graphics::par(mfrow = c(1,1))

  graphics::hist(Tn_star_val, main = "Histogram of Tn*",
                 xlim = c(min(c(Tn_star_val, Tn_val)), max(c(Tn_star_val, Tn_val))),
                 breaks = 30)
  graphics::abline(v = Tn_val, col = "blue")
  graphics::abline(v = stats::quantile(Tn_star_val, 1-alpha), col="red")
}

#' @title summary generic for S3 testResult class
#'
#' @param object testResult object
#' @param ... additional parameters for printing (might become relevant in future versions)
#'
#' @export
#' @examples
#' res <- new_testResult(1, c(0.5, 1.5), 0, 0.5, "equality", 2, 0.05, 0.1, 0.1)
#' summary(res)
summary.testResult <- function(object, ...) {
  x <- object
  cat("Resampling Test for", x$hypothesis, "Hypothesis. \n")
  cat("=========================================\n")
  cat("Resampling iterations:", x$B, "\n")
  cat("alpha:", x$alpha, "\n")
  cat("used kernel-bandwith:", "\n", "h1:", x$h1, "h2:", x$h2)
  cat("\n\nResults \n-----------------------------------------\n")
  cat("Tn:", x$Tn, "\n")
  cat("p:", x$p_value, "\n")
  cat("Decision:", if (x$decision )"Rejected H0" else "Accepted H0", "\n")
}

