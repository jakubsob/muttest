source(file.path("..", "..", "R", "aggregate.R"))

test_that("has_positive works on all-positive input", {
  expect_true(has_positive(c(1, 2, 3)))
})

test_that("all_positive works on all-positive input", {
  expect_true(all_positive(c(1, 2, 3)))
})

test_that("range_width is non-negative", {
  expect_gte(range_width(c(1, 5, 3)), 0)
})

test_that("total is numeric", {
  expect_true(is.numeric(total(c(1, 2, 3))))
})
