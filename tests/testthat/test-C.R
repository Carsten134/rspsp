test_that("C works for simple case", {
  K <- MA_coef_all(.7)
  expect_equal(C(0,0, K), 1)
})

test_that("C works for simple case l=k=2", {
  K <- MA_coef_all(.7)
  result <- .7^2/ sum(K^2)
  expect_equal(C(2,2, K), result)
})


test_that("C works for simple case l = k = 1", {
  K <- MA_coef_all(.7)
  result <- (2*.7^2 + 2*.7)/ sum(K^2)
  expect_equal(C(1,1,K), result)
})

test_that("C works for row", {
  K <- MA_coef_row(.7)
  expect_equal(C(1,0,K), 0)
})
