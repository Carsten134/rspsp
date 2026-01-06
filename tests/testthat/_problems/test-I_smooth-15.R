# Extracted from test-I_smooth.R:15

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rspsp", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
mse_hat <- function(est, f) {
  vals_est <- evaluate(est)$z
  vals_f <- evaluate(f)$z

  return(sum((vals_est - vals_f)^2)/length(vals_est))
}

# test -------------------------------------------------------------------------
K <- MA_coef_all(.7)
spec_est <- I_smooth(I(gridMA(100, 100, K)))
spec_true <- f_true_MA(K)
expect_lt(mse_hat(spec_est, spec_true), .02)
