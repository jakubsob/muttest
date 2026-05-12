source(file.path("..", "..", "R", "label.R"))

test_that("greet returns a non-empty string", {
  expect_true(is.character(greet("Alice")))
  expect_true(nchar(greet("Alice")) > 0)
})

test_that("default_label returns a string", {
  expect_true(is.character(default_label("foo")))
})
