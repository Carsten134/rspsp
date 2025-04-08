test_that("Riemann approximation has sufficiently small error for polynomial", {
  # test if integral of quadratic polynomial is
  # computed correctly and with an error not exceeding .1.
  f <- function(x,y) x^2 + y^2
  true_value <- (8/3)*pi^4
  approximation_error <- abs(riemann_approx(f, 200, 200) - true_value)
  expect_lt(approximation_error, .05)
})
