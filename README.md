
<!-- README.md is generated from README.Rmd. Please edit that file -->

# muttest <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/muttest)](https://CRAN.R-project.org/package=muttest)
[![R-CMD-check](https://github.com/jakubsob/muttest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jakubsob/muttest/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jakubsob/muttest/graph/badge.svg)](https://app.codecov.io/gh/jakubsob/muttest)
[![cucumber](https://img.shields.io/github/actions/workflow/status/jakubsob/muttest/test-acceptance.yaml?branch=main&label=cucumber&logo=cucumber&color=23D96C&labelColor=0f2a13)](https://github.com/jakubsob/muttest/actions/workflows/test-acceptance.yaml)
[![muttest](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/jakubsob/muttest/badges/.badges/main/muttest.json)](https://github.com/jakubsob/muttest/actions/workflows/test-mutation.yaml)
[![Grand
total](http://cranlogs.r-pkg.org/badges/grand-total/muttest)](https://cran.r-project.org/package=muttest)
[![Last
month](http://cranlogs.r-pkg.org/badges/last-month/muttest)](https://cran.r-project.org/package=muttest)
<!-- badges: end -->

Coverage tells you which lines ran. It says nothing about whether your
tests would catch a bug. You can delete every assertion, run `covr`, and
still see 100%.

**{muttest}** measures the quality of your tests — not just how much
code they execute.

## The problem with coverage alone

[covr](https://github.com/r-lib/covr) tells you which lines were
executed. It cannot tell you whether your assertions are strong enough
to catch a real bug. A test suite full of `expect_true(is.numeric(x))`
checks will reach 100% coverage while missing every meaningful failure.

Mutation testing addresses this gap by asking a harder question: *if
this code were subtly wrong, would your tests notice?*

## The need for mutation testing in the age of LLMs

Many teams now use LLMs to write their tests. LLMs are good at producing
syntactically correct, passing tests quickly — but they might cover only
the obvious cases and miss the boundaries:

``` r
# What an LLM may write for is_adult():
test_that("is_adult works", {
  expect_true(is.numeric(is_adult(25))) # checks return type, not logic
  expect_true(is_adult(25))             # clearly an adult
  expect_false(is_adult(10))            # clearly a minor
})

# What actually catches the >= vs > boundary bug:
test_that("is_adult handles the boundary age", {
  expect_true(is_adult(18))    # kills the >= → > mutant
})
```

Both test suites pass. Both have 100% coverage. Only one would catch a
developer accidentally writing `age > 18` instead of `age >= 18`.

Mutation testing gives you a score that reflects **assertion quality**,
not just execution. It gives you a concrete way to understand the real
strength — and the real gaps — in an LLM-generated test suite.

## How it works

- Define a set of code changes (mutations).
- Run your test suite against mutated versions of your source code.
- Measure how often the mutations are caught (i.e., cause test
  failures).

This reveals whether your tests are asserting the right things:

- 0% score → Your tests pass no matter what changes. Your assertions are
  weak.
- 100% score → Every mutation triggers a test failure. Your tests are
  robust.

{muttest} not only gives you the score, but it also tells you which
files need stronger assertions.

# Example

Given our codebase is:

``` r
#' R/is_adult.R
is_adult <- function(age) {
  age >= 18
}
```

And our tests are:

``` r
#' tests/testthat/test-is_adult.R
test_that("is_adult returns TRUE for adults", {
  expect_true(is_adult(25))
})

test_that("is_adult returns FALSE for minors", {
  expect_false(is_adult(10))
})
```

When running `muttest::muttest()` we’ll get a report of the mutation
score:

``` r
withr::with_dir(system.file("examples", "boundary", package = "muttest"), {
  plan <- muttest::muttest_plan(
    mutators = muttest::comparison_operators()
  )
  muttest::muttest(plan)
})
#> ℹ Mutation Testing
#>   |   K |   S |   E |   T |   % | Mutator  | File 
#> ✔ |   1 |   0 |   0 |   1 | 100 | >= → <=  | is_adult.R 
#> x |   1 |   1 |   0 |   2 |  50 | >= → >   | is_adult.R 
#> 
#> Duration: 1.99 s
#> 
#> ── Survived Mutants ────────────────────────────────────────────────────────────
#> is_adult.R  >= → >
#>   2-   age >= 18
#>   2+   age > 18
#> 
#> ── Results ─────────────────────────────────────────────────────────────────────
#> [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]
```

The mutation score is:
$\text{Mutation Score} = \frac{\text{Killed Mutants}}{\text{Total Mutants}} \times 100\%$,
where a Mutant is defined as variant of the original code that is used
to test the robustness of the test suite.

`comparison_operators()` generates mutants by swapping each comparison
operator for related alternatives. For `>=` it produces two mutants:

``` r
#' R/is_adult.R — mutant 1: ">=" -> ">"
is_adult <- function(age) {
  age > 18
}
```

``` r
#' R/is_adult.R — mutant 2: ">=" -> "<="
is_adult <- function(age) {
  age <= 18
}
```

Tests are run against both mutants.

Mutant 2 (`>=` → `<=`) is **killed**: `is_adult(25)` now returns
`FALSE`, which fails the first test.

Mutant 1 (`>=` → `>`) **survives**: `is_adult(25)` still returns `TRUE`
and `is_adult(10)` still returns `FALSE` — the boundary value `18` is
never tested, so the test suite cannot tell `>=` from `>`.

``` r
#' tests/testthat/test-is_adult.R
test_that("is_adult returns TRUE for adults", {
  # ✔ Kills mutant 2 (<=): is_adult(25) returns FALSE
  # 🟢 Doesn't kill mutant 1 (>): is_adult(25) still returns TRUE
  expect_true(is_adult(25))
})

test_that("is_adult returns FALSE for minors", {
  # 🟢 Doesn't kill mutant 1 (>): is_adult(10) still returns FALSE
  # 🟢 Doesn't kill mutant 2 (<=): is_adult(10) returns TRUE → killed by first test anyway
  expect_false(is_adult(10))
})
```

We have killed 1 mutant out of 2, so the mutation score is 50%. The
survivor tells us exactly what to fix — add a test at the boundary:

``` r
test_that("is_adult returns TRUE at the boundary age", {
  expect_true(is_adult(18))  # kills mutant 1: age > 18 returns FALSE for age = 18
})
```

With this test added the score reaches 100%.

# Available mutators

A mutator describes one kind of code change. Pass a list of mutators to
`muttest_plan()` to control what gets mutated.

**Individual mutators**

| Function | Description | Example |
|:---|:---|:---|
| `operator()` | Mutate a binary operator | `operator("+", "-")`: `a + b` → `a - b` |
| `boolean_literal()` | Mutate a boolean literal | `boolean_literal("TRUE", "FALSE")`: `TRUE` → `FALSE` |
| `na_literal()` | Mutate an NA or NULL literal | `na_literal("NA", "NULL")`: `NA` → `NULL` |
| `call_name()` | Mutate a function call name | `call_name("any", "all")`: `any(x)` → `all(x)` |
| `string_empty()` | Mutate non-empty string literals to the empty string | `string_empty()`: `"hello"` → `""` |
| `string_fill()` | Mutate the empty string literal to a placeholder string | `string_fill()`: `""` → `"mutant"` |
| `numeric_increment()` | Increment numeric literals | `numeric_increment()`: `5` → `6` |
| `numeric_decrement()` | Decrement numeric literals | `numeric_decrement()`: `5` → `4` |
| `index_increment()` | Increment subscript indices | `index_increment()`: `x[i]` → `x[i + 1L]` |
| `index_decrement()` | Decrement subscript indices | `index_decrement()`: `x[i]` → `x[i - 1L]` |
| `negate_condition()` | Negate the condition of if/while statements | `negate_condition()`: `if (x > 0)` → `if (!(x > 0))` |
| `remove_condition_negation()` | Remove negation from the condition of if/while statements | `remove_condition_negation()`: `if (!done)` → `if (done)` |
| `remove_negation()` | Remove logical negation | `remove_negation()`: `!is.na(x)` → `is.na(x)` |
| `replace_return_value()` | Replace the value in explicit return() calls | `replace_return_value()`: `return(x)` → `return(NULL)` |

**Preset collections** — return a ready-made list of mutators

| Function | Description | Example |
|:---|:---|:---|
| `arithmetic_operators()` | Arithmetic operator mutators | `+`↔`-`, `*`↔`/`, `^`→`*`, `%%`→`*`, `%/%`→`/` |
| `comparison_operators()` | Comparison operator mutators | `<`↔`>`, `==`↔`!=`, `<`→`<=`, `>`→`>=` … |
| `logical_operators()` | Logical operator mutators | <code>&&</code>↔<code>\|\|</code>, <code>&</code>↔<code>\|</code> |
| `boolean_literals()` | Boolean literal mutators | `TRUE`↔`FALSE`, `T`↔`F` |
| `na_literals()` | NA and NULL literal mutators | `NA`↔`NULL`, `NA`↔`NA_real_`, `NA`↔`NA_integer_`, `NA`↔`NA_character_` |
| `numeric_literals()` | Numeric literal mutators | `5`→`6`, `5`→`4` |
| `index_mutations()` | Index mutation mutators | `x[i]`→`x[i + 1L]`, `x[i]`→`x[i - 1L]` |
| `string_literals()` | String literal mutators | `"hello"`→`""`, `""`→`"mutant"` |
| `condition_mutations()` | Condition mutation mutators | `if (x)`→`if (!(x))`, `if (!x)`→`if (x)` |

# Where to go next

- `vignette("getting-started", package = "muttest")` — a full worked
  example from zero to a mutation score, including how to interpret and
  improve results.
- `vignette("mutation-testing-101", package = "muttest")` — conceptual
  background, the LLM-tests problem in depth, and when mutation testing
  pays off.
- `vignette("mutators", package = "muttest")` — all available mutators,
  when to use each, and how to build custom pairs.
- `vignette("interpreting-results", package = "muttest")` — how to read
  surviving mutants and turn them into stronger tests.
- `vignette("ci-integration", package = "muttest")` — run mutation tests
  on every push, add a score badge, and enforce thresholds.
