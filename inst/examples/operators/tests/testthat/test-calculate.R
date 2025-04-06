test_that("calculate returns a numeric", {
  expect_true(is.numeric(calculate(2, 2))) # ❌ This assertion doesn't kill mutants
})

test_that("calculate always returns 0", {
  expect_equal(calculate(2, 2), 0) # ✅ This assertion only kills "*" -> "/" mutant
})
