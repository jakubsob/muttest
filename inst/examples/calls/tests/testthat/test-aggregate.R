source(file.path("..", "..", "R", "aggregate.R"))

test_that("has_positive returns TRUE when any element is positive", {
  expect_true(has_positive(c(-1, 0, 1)))
})

test_that("has_positive returns FALSE when no element is positive", {
  expect_false(has_positive(c(-2, -1, 0)))
})

test_that("all_positive returns TRUE when all elements are positive", {
  expect_true(all_positive(c(1, 2, 3)))
})

test_that("all_positive returns FALSE when any element is not positive", {
  expect_false(all_positive(c(1, -1, 3)))
})

test_that("range_width returns correct width", {
  expect_equal(range_width(c(1, 5, 3)), 4)
})

test_that("total sums correctly", {
  expect_equal(total(c(1, 2, 3)), 6)
})
