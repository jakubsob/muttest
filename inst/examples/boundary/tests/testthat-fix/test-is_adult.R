test_that("is_adult returns TRUE for adults", {
  expect_true(is_adult(25))
})

test_that("is_adult returns FALSE for minors", {
  expect_false(is_adult(10))
})

test_that("is_adult returns TRUE at the boundary age", {
  expect_true(is_adult(18))
})
