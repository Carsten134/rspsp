test_that("Tn_star returns non-negative numeric", {
  K <- MA_coef_all(.7)

  x <- gridMA(20, 20, K)
  y <- gridMA(20, 20, K)

  expect_gt(Tn_star(x,y), 0)
})


test_that("Tn_star returns a value different from Tn", {
  K <- MA_coef_all(.7)

  x <- gridMA(20, 20, K)
  y <- gridMA(20, 20, K)

  expect_true(Tn(x,y) != Tn_star(x,y))
})

