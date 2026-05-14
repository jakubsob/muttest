# Getting Started with muttest

This guide walks you through your first mutation test run, from setup to
interpreting results.

## Prerequisites

`muttest` works with R packages with `testthat` tests out of the box. If
you’re not using a package structure, or not using `testthat`, see
[`?TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md)
for configuration options.

## Step 1 — Pick a source file to mutate

Start small. Choose one file from `R/` that contains meaningful logic —
branching, arithmetic, comparisons. Avoid files that are mostly glue
code or just call other functions.

## Step 2 — Define a test plan

A test plan describes *what* to mutate and *which* mutations to apply:

``` r

library(muttest)

plan <- muttest_plan(
  source_files = "R/is_adult.R",
  mutators = comparison_operators()
)
```

[`comparison_operators()`](https://jakubsob.github.io/muttest/reference/comparison_operators.md)
is a preset that generates mutants by swapping each comparison operator
for related alternatives. For `>=` it produces two mutants: `>=` → `>`
and `>=` → `<=`.

## Step 3 — Run the tests

``` r

muttest::muttest(plan, "tests/testthat")
```

## Step 4 — Read the output and improve your tests

Each column in the progress table means:

| Column | Meaning                                         |
|--------|-------------------------------------------------|
| K      | Killed — mutants your tests caught              |
| S      | Survived — mutants your tests missed            |
| E      | Errors — mutants that caused unexpected errors  |
| T      | Total mutants for this mutator/file combination |
| %      | Mutation score for this row                     |

The **mutation score** is `Killed / Total × 100%`. A `✔` row means at
least one mutant was killed; an `x` row means all mutants survived.

Here is a complete example showing a weak test, the live output, and the
fix:

### Example: `is_adult` — Missing Boundary Value

A test suite that checks only values clearly on one side of a threshold
will let boundary-shift comparison mutants survive. The surviving mutant
names the exact input your tests have never exercised. Adding a test at
that precise boundary kills it.

#### The function

``` r

is_adult <- function(age) {
  age >= 18
}
```

`is_adult` returns `TRUE` when age is 18 or older, using `>=`. Swapping
to `>` would incorrectly classify an 18-year-old as a minor.

#### Weak test

``` r

test_that("is_adult returns TRUE for adults", {
  expect_true(is_adult(25))
})

test_that("is_adult returns FALSE for minors", {
  expect_false(is_adult(10))
})
```

The tests check age 25 (clearly adult) and age 10 (clearly minor). Both
inputs return the same result whether the operator is `>=` or `>`, so
the tests cannot tell the operators apart.

**Mutation testing output:**

    ℹ Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   1 | 100 | >= → <=  | is_adult.R 
    x |   1 |   1 |   0 |   2 |  50 | >= → >   | is_adult.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

`>= → >` survives. Changing `age >= 18` to `age > 18` only affects one
input — `age = 18` exactly. The tests never pass 18, so the function
behaves identically for 25 and 10 under both operators.

#### The fix

``` r

test_that("is_adult returns TRUE for adults", {
  expect_true(is_adult(25))
})

test_that("is_adult returns FALSE for minors", {
  expect_false(is_adult(10))
})

test_that("is_adult returns TRUE at the boundary age", {
  expect_true(is_adult(18))
})
```

Add a test at the boundary value 18. With `>= 18`, the condition is
`TRUE` and the function returns `TRUE`. With `> 18`, it is `FALSE` and
returns `FALSE`. This difference kills the mutant.

**After the fix:**

    ℹ Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   1 | 100 | >= → <=  | is_adult.R 
    ✔ |   2 |   0 |   0 |   2 | 100 | >= → >   | is_adult.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 2 | SURVIVED 0 | ERRORS 0 | TOTAL 2 | SCORE 100.0% ]

> When a comparison mutant survives, find the boundary value implied by
> the operator and add a test that passes exactly that value.

## The feedback loop

1.  Write tests
2.  Run muttest
3.  Find survivors
4.  Strengthen tests
5.  Repeat

Start with one file. Aim for a meaningful score improvement each
iteration rather than chasing 100% immediately. A score of 80%+ on
critical business logic can be a reasonable target to start from.

## Next steps

- [What is Mutation
  Testing?](https://jakubsob.github.io/muttest/articles/mutation-testing-101.md)
  — conceptual background and when mutation testing pays off most
- [Mutator
  Reference](https://jakubsob.github.io/muttest/articles/mutators.md) —
  all available mutators and how to pick the right ones
- [CI
  Integration](https://jakubsob.github.io/muttest/articles/ci-integration.md)
  — run mutation tests automatically on every push
