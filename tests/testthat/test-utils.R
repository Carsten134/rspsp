test_that("fourier_freq returns the same length as adviced", {
  expect_equal(length(fourier_freq(5)), 5)
})

test_that("fourier_freq is numeric", {
  expect_true(is.numeric(fourier_freq(5)))
})

test_that("MA_coef_row util works for simple case", {
  K0 <- matrix(0, 3, 3)
  K0[2,] <- .7
  K0[2,2] <- 1

  K <- MA_coef_row(.7)
  expect_equal(K, K0)
})

test_that("MA_coef_col util works for simple case", {
  K0 <- matrix(0, 3, 3)
  K0[,2] <- .7
  K0[2,2] <- 1

  K <- MA_coef_col(.7)
  expect_equal(K, K0)
})

test_that("MA_coef_all util works for simple case", {
  K0 <- matrix(.7, 3, 3)
  K0[2,2] <- 1

  K <- MA_coef_all(.7)
  expect_equal(K, K0)
})
