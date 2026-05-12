source(file.path("..", "..", "R", "guard.R"))

test_that("is_valid returns FALSE for NULL", {
  expect_false(is_valid(NULL))
})

test_that("is_valid returns TRUE for non-NULL", {
  expect_true(is_valid(1))
  expect_true(is_valid("hello"))
})

test_that("all_valid returns TRUE for non-NULL list", {
  expect_true(all_valid(list(1, 2, 3)))
})

test_that("all_valid returns FALSE when any element is NULL", {
  expect_false(all_valid(list(1, NULL, 3)))
})
