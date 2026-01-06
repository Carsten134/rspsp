
test_that("gridMA works", {
  K <- MA_coef_all(0.3)
  x <- gridMA(10, 10, K)
  expect_equal(dim(x), c(10, 10))
  expect_true(is.matrix(x))
  expect_true(is.numeric(x))
})

test_that("gridAR works", {
  x <- gridAR(10, 10, 0.8, 0.1, 0.05)
  expect_equal(dim(x), c(10, 10))
  expect_true(is.matrix(x))
  expect_true(is.numeric(x))
  
  expect_error(gridAR(-1, 10, 0.8, 0.1, 0.05))
  expect_error(gridAR(10, -1, 0.8, 0.1, 0.05))
})

test_that("mvgridMA works", {
  K <- MA_coef_all(.7)
  K_off <- K * .5
  
  res <- mvgridMA(10, 10, K, K, K_off, K_off, .4)
  
  expect_type(res, "list")
  expect_equal(names(res), c("X1", "X2"))
  expect_equal(dim(res$X1), c(10, 10))
  expect_equal(dim(res$X2), c(10, 10))
})
