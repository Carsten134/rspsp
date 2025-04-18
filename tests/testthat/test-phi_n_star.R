test_that("phi_n_star rejects easy case", {
  x <- gridMA(20, 20, MA_coef_row(.7))
  y <- gridMA(20, 20, MA_coef_col(.7))

  expect_equal(phi_n_star(x, y, 50, .05, print_result = F)$decision, 1)
})
