source(file.path("..", "..", "R", "classify.R"))

test_that("sign_of returns a string", {
  expect_true(is.character(sign_of(5)))
})

test_that("clamp returns a number", {
  expect_true(is.numeric(clamp(7, 5, 10)))
})
