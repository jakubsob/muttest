source(file.path("..", "..", "R", "calculate.R"))

test_that("calculate returns x * y + x", {
  expect_equal(calculate(2, 3), 8)
})
