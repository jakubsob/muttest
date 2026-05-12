source(file.path("..", "..", "R", "label.R"))

test_that("greet returns greeting with name", {
  expect_equal(greet("Alice"), "Hello, Alice!")
})

test_that("greet returns stranger message for empty name", {
  expect_equal(greet(""), "Hello, stranger!")
})

test_that("default_label returns label when non-empty", {
  expect_equal(default_label("foo"), "foo")
})

test_that("default_label returns unknown for empty string", {
  expect_equal(default_label(""), "unknown")
})
