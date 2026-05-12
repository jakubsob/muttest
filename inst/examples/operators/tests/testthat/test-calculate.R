source(file.path("..", "..", "R", "calculate.R"))

test_that("calculate returns a numeric", {
  expect_true(is.numeric(calculate(2, 3)))
})
