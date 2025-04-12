test_that("calculate2 returns a numeric", {
  expect_true(is.numeric(calculate2(2, 2))) # ❌ This assertion doesn't kill mutants
})

test_that("calculate2 always returns 0", {
  expect_equal(calculate2(2, 2), 0) # ✅ This assertion only kills "*" -> "/" mutant
})
