# context with one test that groups expectations
context("Test for range value") 

test_that("range works as expected", {
  x <- c(1, 2, 4, 2, 7)
  
  expect_type(range_value(x), 'double')
  expect_length(range_value(x), 1)
  expect_equal(range_value(x), 6)
  
  y <- c(1, 2, 5, 8, NA)
  expect_length(range_value(y), 1)
  expect_equal(range_value(y, na.rm = TRUE), 7)
  # expect_length(range_value(x), 1)
  
})
context("Test for missing value") 


test_that("missing value", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, NA)
  z <- c(1, NA, 3, 4, NA)
  nas <- rep(NA, 10)
  
  expect_equal(missing_values(x), 0)
  expect_gte(missing_values(x), 0)
  expect_length(missing_values(z), 1)
  expect_equal(missing_values(y), 1)
  expect_length(missing_values(x), 1)
  expect_length(missing_values(y), 1)
  expect_equal(missing_values(z), 2)
  expect_equal(missing_values(nas), 10)
})
test_that("range value for logical vectors", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(range_value(z), 1L)
  expect_length(range_value(z), 1)
  expect_type(range_value(z), 'integer')
})

test_that("range value for numeric vectors with NAs", {
  y <- c(1, 2, 3, 5, 7, NA)
  
  expect_equal(range_value(y), NA_real_)
  expect_length(range_value(y), 1)
})




test_that("range value stops for character vectors", {
  w <- letters[1:5]
  
  expect_error(range_value(w))
})


