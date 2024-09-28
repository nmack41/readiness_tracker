library(testthat)
source("C:/Projects/readiness_tracker/app.R")

test_that("validate_inputs works correctly", {
  expect_true(validate_inputs(70, 50, 5)$valid)
  expect_false(validate_inputs(-1, 50, 5)$valid)
  expect_false(validate_inputs(70, 101, 5)$valid)
  expect_false(validate_inputs(70, 50, 11)$valid)
})

test_that("insert_data handles errors", {
  # This test assumes the insert_data function will fail due to invalid credentials
  result <- insert_data(Sys.time(), 70, 50, 5)
  expect_false(result$success)
  expect_true(grepl("error", tolower(result$message)))
})