source(file.path("..", "..", "R", "aggregate.R"))

test_that("has_positive is TRUE when at least one is positive", {
  expect_true(has_positive(c(-1, 0, 1)))
})

test_that("has_positive is FALSE when all are non-positive", {
  expect_false(has_positive(c(-2, -1, 0)))
})

test_that("all_positive distinguishes partial from full positivity", {
  expect_true(all_positive(c(1, 2, 3)))
  expect_false(all_positive(c(1, -1, 3)))
})

test_that("range_width equals max minus min", {
  expect_equal(range_width(c(1, 5, 3)), 4)
})

test_that("total equals sum of elements", {
  expect_equal(total(c(1, 2, 4)), 7)
})
