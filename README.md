
# muttest

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/muttest)](https://CRAN.R-project.org/package=muttest)
[![R-CMD-check](https://github.com/jakubsob/muttest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jakubsob/muttest/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/jakubsob/muttest/graph/badge.svg)](https://app.codecov.io/gh/jakubsob/muttest)
<!-- badges: end -->

Mutation testing frameworks work by intentionally introducing small changes (called mutations) into your code to check whether your tests can detect them.

The goal is to evaluate how effective your test suite is — if your tests fail when the code is altered, that’s a good sign. If they don’t, your tests might be missing something important.

# Example

Given our codebase is:

```r
#' R/calculate.R
calculate <- function(x, y) {
  (x + y) * 0
}
```

And our tests are:

```r
#' tests/testthat/test_calculate.R
test_that("calculate always returns 0", {
  expect_equal(calculate(2, 2), 0)
})

test_that("calculate returns a numeric", {
  expect_true(is.numeric(calculate(2, 2)))
})
```

When running `muttest::test()` we'll get a report of the mutation score:
```r
muttest::test(
  path = "tests/testthat",
  source_path = "R",
  mutators = list(operator("+", "-"), operator("*", "/"))
)
#> v Mutation score: 50%
```

The mutation score is: $\text{Mutation Score} = \frac{\text{Killed Mutants}}{\text{Total Mutants}} \times 100\%$, where a Mutant is defined as variant of the original code that is used to test the robustness of the test suite.

In the example there were 2 mutants of the code:

```r
#' R/calculate.R
calculate <- function(x, y) {
  (x - y) * 0 # mutant 1: "+" -> "-"
}
```

```r
#' R/calculate.R
calculate <- function(x, y) {
  (x + y) / 0 # mutant 2: "*" -> "/"
}
```

Tests are run against both variants of the code.

The first test run against the first mutant will pass, because the result is still 0. The second test run against the second mutant will fail, because the result is Inf.

The second test will pass against both mutants, because the result is still numeric.

```r
#' tests/testthat/test_calculate.R
test_that("calculate always returns 0", {
  # 🟢 This test doesn't kill "+" -> "-" operator mutant: (2 - 2) * 0 = 0
  # ❌ This test kills "*" -> "/" operator mutant: (2 + 2) / 0 = Inf
  expect_equal(calculate(2, 2), 0)
})

test_that("calculate returns a numeric", {
  # 🟢 This test doesn't kill "+" -> "-", (2 - 2) * 0 = 0, is numeric
  # 🟢 This test doesn't kill "*" -> "/", (2 + 2) / 0 = Inf, is numeric
  expect_true(is.numeric(calculate(2, 2)))
})
```

We have killed 1 mutant out of 2, so the mutation score is 50%.
