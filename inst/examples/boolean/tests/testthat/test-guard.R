source(file.path("..", "..", "R", "guard.R"))

test_that("is_valid returns a logical", {
  expect_true(is.logical(is_valid(NULL)))
  expect_true(is.logical(is_valid(42)))
})

test_that("all_valid returns a logical", {
  expect_true(is.logical(all_valid(list(1, 2))))
})
