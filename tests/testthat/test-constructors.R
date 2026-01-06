
test_that("new_testResult works", {
  res <- new_testResult(1.5, c(1, 2), 0, 0.5, "equality", 100, 0.05, 0.1, 0.1)
  expect_s3_class(res, "testResult")
})

test_that("new_periodoTestResult works", {
  res <- new_periodoTestResult(1.2, 0.1, 0, 0.05, "equality")
  expect_s3_class(res, "periodoTestResult")
})
