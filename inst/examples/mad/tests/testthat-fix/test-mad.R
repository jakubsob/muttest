source(file.path("..", "..", "R", "mad.R"))

test_that("mean absolute deviation equals average distance from center", {
  expect_equal(mean_absolute_deviation(c(1, 3, 5), 3), 4 / 3)
})
