# tests/testthat/test-summarise_metrics.R

library(testthat)
library(dplyr)

# Helper function to create sample data
create_test_df <- function() {
  tibble(
    impressions = c(1000, 2000, 3000, NA),
    clicks = c(50, 100, 150, NA),
    spend = c(100, 200, 300, NA),
    pageviews = c(75, 150, 225, NA)
  )
}

test_that("summarise_metrics handles basic case correctly", {
  test_df <- create_test_df()

  result <- summarise_metrics(test_df)

  expect_equal(result$impressions, 6000)
  expect_equal(result$clicks, 300)
  expect_equal(result$spend, 600)
  expect_equal(result$pageviews, 450)
  expect_equal(result$cpc, 2)  # 600/300
  expect_equal(result$cpm, 100)  # (600/6000)*1000
  expect_equal(result$cppv, 1.333333, tolerance = 0.0001)  # 600/450
  expect_equal(result$ctr, 0.05)  # 300/6000
})

test_that("summarise_metrics checks for required columns", {
  # Missing required column
  bad_df <- tibble(
    impressions = c(1000, 2000),
    clicks = c(50, 100)
    # Missing spend and pageviews
  )

  expect_error(summarise_metrics(bad_df))
})

test_that("summarise_metrics handles NA values correctly", {
  test_df <- create_test_df()  # Already includes NAs

  result <- summarise_metrics(test_df)

  # Check that NAs are properly removed in calculations
  expect_false(any(is.na(result)))
})

test_that("summarise_metrics handles zero clicks/impressions", {
  zero_df <- tibble(
    impressions = c(0, 0),
    clicks = c(0, 0),
    spend = c(100, 200),
    pageviews = c(75, 150)
  )

  result <- summarise_metrics(zero_df)

  expect_true(is.infinite(result$cpc))  # Division by zero
  expect_true(is.infinite(result$cpm))  # Division by zero
  expect_equal(result$spend, 300)
})


test_that("summarise_metrics returns a dataframe", {
  test_df <- create_test_df()  # Already includes NAs

  result <- summarise_metrics(test_df)


  expect_equal(is.data.frame(result), TRUE)
})
