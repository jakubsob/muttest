source(file.path("..", "..", "R", "access.R"))

test_that("access control works", {
  expect_true(can_access(TRUE, TRUE))
  expect_false(can_access(FALSE, FALSE))
})
