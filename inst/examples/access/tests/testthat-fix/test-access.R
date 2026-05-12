source(file.path("..", "..", "R", "access.R"))

test_that("access control works", {
  expect_true(can_access(TRUE, TRUE))
  expect_false(can_access(FALSE, FALSE))
})

test_that("owner-only access is granted", {
  expect_true(can_access(FALSE, TRUE))
})

test_that("admin-only access is granted", {
  expect_true(can_access(TRUE, FALSE))
})
