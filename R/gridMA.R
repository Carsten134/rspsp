#' @import ravetools
#' @export
#' @title simulating spatial MA process
#' @description Um unsere Implementierung validieren zu können, brauchen wir Daten, welche sowohl unter \eqn{H_0} als auch unter der Alternative erzeugt wurden. Wie bereits in der Einleitung erwähnt, wollen wir alles so simpel wie möglich halten. Der Prozess ist daher eine einfache Linearkombination von normalverteilten Variablen und stark von der Simulation eines \eqn{MA(q)} Prozesses inspiriert. Der Grundgedanke hierbei ist, dass wir die Koeffizienten \eqn{\varphi_k} des Lag-Polynoms reinterpretieren von "linearer Einfluss von Residuen zu Lag \eqn{k}" zu "linearer Einfluss von Residuen zum Abstand \eqn{k}". Also heißt dass für \eqn{X_{ij}}, dass jedes Residuum mit Abstand \eqn{k} einen linearen Einfluss von \eqn{\varphi_k} auf \eqn{X_{ij}} nimmt. Dieser Prozess lässt sich mit einer 2D Konvolution umsetzen und ist nicht nur stationär, sondern hat auch eine isotrope Kovarianzfunktion.
#'
#' @param N rows
#' @param M columns of grid
#' @param distribution Type of distribution must be one of "normal", "uniform", "cauchy", "chisq"
#'
#' @return matrix with N rows and M columns
gridMA <- function(N, M, K, padding = nrow(K) %/% 2, distribution="normal") {
  N_tilde <- N + 2 * padding
  M_tilde <- M + 2 * padding
  n <- N_tilde * M_tilde

  eps_val <- switch(distribution,
                    normal=rnorm(n),
                    uniform=runif(n),
                    cauchy=rcauchy(n),
                    chisq=rchisq(n, 3))

  eps <- matrix(eps_val,
                nrow = M_tilde,
                ncol = N_tilde)

  x <- ravetools::convolve_image(eps, K)
  # cut padding
  return(x[(padding+1):(M+padding),(padding+1):(N+padding)])
}
