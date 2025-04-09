test_that("Tn returns numeric which is positive", {
  K <- MA_coef_all(.7)

  x <- gridMA(20, 20, K)
  y <- gridMA(20, 20, K)

  expect_gt(Tn(x,y), 0)
})
