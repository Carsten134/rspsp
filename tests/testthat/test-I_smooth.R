mse_hat <- function(est, f) {
  vals_est <- evaluate(est)$z
  vals_f <- evaluate(f)$z

  return(sum((vals_est - vals_f)^2)/length(vals_est))
}


test_that("Smoothed periodogramm approximates spectral density", {
  K <- MA_coef_all(.7)

  spec_est <- I_smooth(I(gridMA(100, 100, K)))
  spec_true <- f_true_MA(K)

  expect_lt(mse_hat(spec_est, spec_true), .05)
})


test_that("Smoothed periodogramm approximates col spectral density", {
  K <- MA_coef_col(.7)

  spec_est <- I_smooth(I(gridMA(100, 100, K)))
  spec_true <- f_true_MA(K)

  expect_lt(mse_hat(spec_est, spec_true), .05)
})


test_that("Smoothed periodogramm approximates row spectral density", {
  K <- MA_coef_row(.7)
  spec_est <- I_smooth(I(gridMA(100, 100, K)))
  spec_true <- f_true_MA(K)
  expect_lt(mse_hat(spec_est, spec_true), .05)
})

test_that("All funcitonal values of the smoothed periodogramm are non-negative",{
  K <- MA_coef_all(.7)
  spec_est <- I_smooth(I(gridMA(100, 100, K)))
  z <- evaluate.grid_function(spec_est)$z
  expect_true(all(z >= 0))
})


