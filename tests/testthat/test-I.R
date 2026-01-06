
test_that("I works for simple case", {
  x <- matrix(rnorm(100), 10, 10)
  per <- I(x)
  expect_equal(dim(per), dim(x))
  expect_true(is.numeric(per))
  expect_true(all(per >= 0))
})


test_that("I works for simple case with MA", {
  K <- MA_coef_all(.7)
  x <- gridMA(20, 20, K)
  per <- I(x)
  expect_equal(dim(per), dim(x))
  expect_true(is.numeric(per))
  expect_true(all(per >= 0))
})
