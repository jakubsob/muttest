box::use(
  ../../R/calculate[calculate],
)

test_that("calculate", {
  expect_equal(calculate(2, 1), 1.5)
})
