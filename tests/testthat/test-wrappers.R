
test_that("test.periodo works", {
  set.seed(42)
  K0 <- MA_coef_all(0.3)
  x <- gridMA(10, 10, K0)
  y <- gridMA(10, 10, K0)

  res <- test.periodo(x, y, 0.05)
  expect_s3_class(res, "periodoTestResult")
})

test_that("test.spectral equality works", {
  set.seed(42)
  K0 <- MA_coef_all(0.3)
  x <- gridMA(10, 10, K0)
  y <- gridMA(10, 10, K0)

  res <- test.spectral(x, y, 10, 0.05, hypothesis = "equality")
  expect_s3_class(res, "testResult")
})

test_that("test.spectral isotropy works", {
  set.seed(42)
  K0 <- MA_coef_all(0.3)
  x <- gridMA(10, 10, K0)

  res <- test.spectral(x, NULL, 10, 0.05, hypothesis = "isotropy")
  expect_s3_class(res, "testResult")
})

test_that("test.spectral stationary works", {
  set.seed(42)
  K0 <- MA_coef_all(0.3)
  x <- gridMA(10, 10, K0)

  res <- test.spectral(x, NULL, 10, 0.05, hypothesis = "stationary")
  expect_s3_class(res, "testResult")
})

test_that("test.spectral input validation works", {
  expect_error(test.spectral("a", "b", 10, 0.05))
  expect_error(test.spectral(matrix(1, 10, 10), matrix(1, 10, 10), 10.5, 0.05)) # B float
  expect_error(test.spectral(matrix(1, 10, 10), matrix(1, 10, 10), 10, 1.5)) # alpha > 1
})
