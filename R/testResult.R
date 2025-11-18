
#' @title Constructor for testResult object
#'
#' @export
new_testResult <- function(Tn, Tn_star, decision, p_value, hypothesis, B, alpha) {
  stopifnot(is.numeric(Tn))
  stopifnot(is.numeric(Tn_star))
  stopifnot(is.numeric(decision))
  stopifnot(is.numeric(p_value) | p_value < 0 | p_value > 1)
  stopifnot(is.character(hypothesis))
  stopifnot(is.numeric(B))
  stopifnot(is.numeric(alpha) | alpha < 0 | alpha > 1)

  structure(list(
    Tn=Tn,
    Tn_star=Tn_star,
    decision=decision,
    p_value=p_value,
    hypothesis=hypothesis,
    B=B,
    alpha=alpha
  ),
  class="testResult")
}

#' @title Print generic for testResult S3 class
#'
#' @method print testResult
#'
#' @export
print.testResult <- function(x, ...) {
  cat("Test Result for", x$hypothesis, "type", "\n", "-----------", "\n")
  cat("Tn:", x$Tn, "\n")
  cat("p_value:", x$p_value, "\n")
  cat("decision: ", if (x$decision )"Rejected H0" else "Accepted H0", "\n")
}

#' @title Print generic for testResult S3 class
#'
#' @method plot testResult
#'
#' @export
plot.testResult <- function(x, ...) {
  Tn_star_val <- x$Tn_star
  decision <- x$decision
  Tn_val <- x$Tn
  alpha <- x$alpha
  par(mfrow = c(1,1))

  hist(Tn_star_val, main = "Histogram of Tn*",
       xlim = c(min(c(Tn_star_val, Tn_val)), max(c(Tn_star_val, Tn_val))),
       breaks = 30)
  abline(v = Tn_val, col = "blue")
  abline(v = quanitle(Tn_star_val, 1-alpha))
}

