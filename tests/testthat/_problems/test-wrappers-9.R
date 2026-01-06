# Extracted from test-wrappers.R:9

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rspsp", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
set.seed(42)
K0 <- MA_coef_all(0.3)
x <- gridMA(10, 10, K0)
y <- gridMA(10, 10, K0)
res <- test.periodo(x, y, 0.05)
expect_s3_class(res, "testResult")
