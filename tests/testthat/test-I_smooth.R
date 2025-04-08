mse_hat <- function(est, f) {
  vals_est <- evaluate(est)$z
  vals_f <- evaluate(f)$z

  return(sum((vals_est - vals_f)^2)/length(vals_est))
}


test_that("Smoothed Periodogramm approximates spectral density", {
  K <- matrix(.3, 3, 3)
  K[2,2] <- 1

  spec_est <- I_smooth(I(gridMA(250, 250, K)))
  spec_true <- f_true_MA(K)

  expect_lt(mse_hat(spec_est, spec_true), .01)
})
