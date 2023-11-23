test_that("throw errors and warnings", {
  expect_error(TerraclimateR:::show_progress(2, 10),
               "Index 'i' must start at 1.")
  expect_error(TerraclimateR:::show_progress("a", 10),
               "Both arguments 'i' and 'n' must be positive integers.")
  expect_error(TerraclimateR:::show_progress(10, 1),
               "Index 'i' must not be greater than length of data 'n'.")
  expect_warning(TerraclimateR:::show_progress(1.1, 10),
               "Removing decimal places for indexing.")
  expect_error(TerraclimateR:::show_progress(1, 10, 1:10),
               "Argument 'names' must be a character vector.")
})
