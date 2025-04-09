test_that("f_true_MA evaluates as non negative", {
  f <- f_true_MA(MA_coef_all(.7))
  z <- evaluate.grid_function(f)$z
  expect_true(all(z >= 0))
})

test_that("f_true_MA has integral 1", {
  f <- f_true_MA(MA_coef_all(.7))
  expect_equal(riemann_approx(f), 1)
})
