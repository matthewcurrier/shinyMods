
# Test suite
test_that("dataframe contains required columns", {
  # Test case 1: All required columns present
  valid_df <- data.frame(
    TERRITORY = c("North America", "Europe", "Asia"),
    CUSTOMERNAME = letters[1:3],
    ORDERNUMBER = LETTERS[1:3],
    extra_col = c(TRUE, FALSE, TRUE)
  )
  expect_no_error(salesServer("justatest", valid_df))

  # Test case 2: Missing one required column
  invalid_df <- data.frame(
    TERRITORY = "ter1",
    CUSTOMERNAME = "a",
    extra_col = TRUE
  )
  expect_error(
    salesServer("justatest", invalid_df),
    "DataFrame must contain columns: TERRITORY, CUSTOMERNAME, ORDERNUMBER"
  )

  # Test case 3: Check column names directly
  required_cols <- c("TERRITORY", "CUSTOMERNAME", "ORDERNUMBER")
  expect_true(
    all(required_cols %in% names(valid_df)),
    info = "All required columns should be present"
  )

  # Alternative approach using has_name()
  for (col in required_cols) {
    expect_true(
      has_name(valid_df, col),
      info = sprintf("DataFrame should have column '%s'", col)
    )
  }
})
