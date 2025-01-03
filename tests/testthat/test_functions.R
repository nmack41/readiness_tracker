# FILE: test_functions.R

library(testthat)
library(httr)
library(jsonlite)
library(dotenv)

# Load environment variables
load_dot_env()

# Mock environment variables
Sys.setenv(SUPABASE_API_KEY = "test_api_key")
Sys.setenv(SUPABASE_URL = "https://test.supabase.co")

# Mock responses
mock_response <- function(status_code, content) {
  structure(
    list(
      status_code = status_code,
      content = function(...) content
    ),
    class = "response"
  )
}

# Test validate_email function
test_that("validate_email works correctly", {
  expect_true(validate_email("test@example.com"))
  expect_false(validate_email("invalid-email"))
})

# Test sign_up function
test_that("sign_up works correctly", {
  with_mock(
    `httr::POST` = function(...) mock_response(200, list(message = "Sign-up successful!")),
    {
      result <- sign_up("test@example.com", "password123")
      expect_equal(result$message, "Sign-up successful!")
    }
  )
  
  with_mock(
    `httr::POST` = function(...) mock_response(400, list(message = "Sign-up failed")),
    {
      expect_error(sign_up("invalid-email", "password123"), "Invalid email format")
      expect_error(sign_up("test@example.com", "password123"), "Sign-up failed")
    }
  )
})

# Test log_in function
test_that("log_in works correctly", {
  with_mock(
    `httr::POST` = function(...) mock_response(200, list(access_token = "test_token", user = list(id = "test_user_id"))),
    {
      result <- log_in("test@example.com", "password123")
      expect_equal(result$access_token, "test_token")
      expect_equal(result$id, "test_user_id")
    }
  )
  
  with_mock(
    `httr::POST` = function(...) mock_response(400, list(message = "Log-in failed")),
    {
      expect_error(log_in("test@example.com", "wrongpassword"), "Log-in failed")
    }
  )
})

# Test get_user_data function
test_that("get_user_data works correctly", {
  with_mock(
    `httr::GET` = function(...) mock_response(200, list(data = "user_data")),
    {
      result <- get_user_data("test_token", "test_user_id")
      expect_equal(result$data, "user_data")
    }
  )
  
  with_mock(
    `httr::GET` = function(...) mock_response(400, list(message = "Failed to fetch training metrics")),
    {
      expect_error(get_user_data("test_token", "test_user_id"), "Failed to fetch training metrics")
    }
  )
})