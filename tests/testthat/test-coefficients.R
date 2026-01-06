
test_that("MA_coef_all works", {
  K <- MA_coef_all(0.5)
  expect_true(is.matrix(K))
  expect_true(is.numeric(K))
})

test_that("MA_coef_row works", {
  K <- MA_coef_row(0.5)
  expect_true(is.matrix(K))
  expect_true(is.numeric(K))
})

test_that("MA_coef_col works", {
  K <- MA_coef_col(0.5)
  expect_true(is.matrix(K))
  expect_true(is.numeric(K))
})
