source(file.path("..", "..", "R", "mad.R"))

test_that("mean absolute deviation is non-negative", {
  expect_gte(mean_absolute_deviation(c(1, 3, 5), 3), 0)
})
