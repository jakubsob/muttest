source(file.path("..", "..", "R", "classify.R"))

test_that("sign_of returns positive for positive numbers", {
  expect_equal(sign_of(5), "positive")
})

test_that("sign_of returns negative for negative numbers", {
  expect_equal(sign_of(-3), "negative")
})

test_that("sign_of returns zero for zero", {
  expect_equal(sign_of(0), "zero")
})

test_that("clamp returns lo when x is below lo", {
  expect_equal(clamp(1, 5, 10), 5)
})

test_that("clamp returns hi when x is above hi", {
  expect_equal(clamp(15, 5, 10), 10)
})

test_that("clamp returns x when x is within bounds", {
  expect_equal(clamp(7, 5, 10), 7)
})
