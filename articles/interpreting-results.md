# Reading mutation results and strengthening tests

The mutation score is only useful if you know how to read it. This
article walks through three real scenarios — one for each operator class
— showing what a surviving mutant looks like in the output, what it
tells you about your tests, and exactly what to change.

## The output table

Every
[`muttest::muttest()`](https://jakubsob.github.io/muttest/reference/muttest.md)
run produces a table with one row per mutator/file pair:

| Column | Meaning |
|----|----|
| K | **Killed** — mutants that caused at least one test to fail. Your tests caught the change. |
| S | **Survived** — mutants that passed all tests. Your tests did not notice the change. |
| E | **Errors** — mutants whose mutated code errored (parse or runtime). The mutation was still detected, so it counts as killed; it is tracked separately so you can see how it was caught. |
| T | **Total** — mutants generated for this mutator/file pair. |
| % | **Score** — `(K + E) / (K + S + E) × 100`. Errors count as detected; mutants with no test coverage are excluded. |

A `✔` row means at least one mutant was killed. An `x` row means every
mutant survived — your tests passed unchanged.

## What the score tells you

| Score | Signal |
|----|----|
| 0% | Your assertions do not constrain the outcome. Tests pass no matter what. |
| 1–99% | Some cases are well-tested; others are not. The survivors point at the gaps. |
| 100% | Every generated mutant was caught. Your tests are robust against these specific changes. |

A 100% score does not mean there are no bugs — it means there are no
untested bugs *of the kinds the mutators probe*. Start with the
survivors; they are the actionable signal.

## [The HTML report](https://jakubsob.github.io/muttest/report/muttest.html)

For anything bigger than a couple of files, review survivors in the HTML
report instead of the terminal. Run muttest with `JSONMutationReporter`,
then render the report:

``` r

muttest(plan, reporter = JSONMutationReporter$new())
report()
```

The report opens filtered to survived mutants, overlays each mutant on
its source line, and shows the exact diff that your tests failed to
notice.

[See a live example
report](https://jakubsob.github.io/muttest/report/muttest.html).

------------------------------------------------------------------------

### Example: Mean Absolute Deviation — Direction-Insensitive Assertion

An assertion that checks a *property* of the result (sign, order,
non-negativity) rather than the *value* will be blind to arithmetic
operator swaps. Both the original and the mutant satisfy the same
directional constraint, so the mutant survives. Replacing the
directional assertion with an exact-value assertion kills it.

#### The function

``` r

mean_absolute_deviation <- function(x, center) {
  mean(abs(x - center))
}
```

`mean_absolute_deviation` computes the average distance from a center
point: `mean(abs(x - center))`. The subtraction `x - center` is the step
that arithmetic operator mutators target.

#### Weak test

``` r

test_that("mean absolute deviation is non-negative", {
  expect_gte(mean_absolute_deviation(c(1, 3, 5), 3), 0)
})
```

The test asserts that the result is non-negative (`expect_gte(..., 0)`).
This checks a property of the output rather than its value.

**Mutation testing output:**

    ℹ Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File 
    x |   0 |   1 |   0 |   0 |   1 |   0 | - → +    | mad.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 0 | SURVIVED 1 | NO COVERAGE 0 | ERRORS 0 | TOTAL 1 | SCORE 0.0% ]

`- → +` survives. Changing `x - center` to `x + center` gives a
different number, but `abs(x + center)` is also non-negative for all
inputs. For `x = c(1,3,5)` and `center = 3`:

- Original: `abs(c(-2, 0, 2))` → mean = 4/3 ≈ 1.33
- Mutant: `abs(c(4, 6, 8))` → mean = 6

Both are ≥ 0. The assertion cannot distinguish them.

This pattern is common in LLM-generated tests that check what is
*obviously true* about the result (it is positive, it is numeric) rather
than what is *specifically correct*.

#### The fix

``` r

test_that("mean absolute deviation equals average distance from center", {
  expect_equal(mean_absolute_deviation(c(1, 3, 5), 3), 4 / 3)
})
```

Replace the directional assertion with `expect_equal` and a value
computed by hand. Now the mutant returns 6, which is not 4/3, and the
test fails.

**After the fix:**

    ℹ Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   0 |   1 | 100 | - → +    | mad.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 1 | SURVIVED 0 | NO COVERAGE 0 | ERRORS 0 | TOTAL 1 | SCORE 100.0% ]

> When an arithmetic mutant survives, replace directional assertions
> (`expect_gt`, `expect_gte`) with exact-value assertions
> (`expect_equal`). Compute the expected value by hand and hard-code it.

------------------------------------------------------------------------

### Example: Shipping Cost — Missing Boundary Value

A test suite that checks only “clearly above” and “clearly below” cases
will let a boundary-shift mutant survive. The surviving mutant names the
exact input your tests have never exercised. Adding a test at that
precise boundary value kills it.

#### The function

``` r

apply_discount <- function(price) {
  price - 1
}
shipping_cost <- function(weight_kg) {
  if (weight_kg > 5) 15.00 else 5.00
}
```

`shipping_cost` returns a flat rate based on whether the package is over
or under 5 kg. The strict `>` operator means a 5 kg package pays the
lower rate; `>=` would charge it the higher rate.

#### Weak test

``` r

test_that("heavy packages cost more than light ones", {
  expect_gt(shipping_cost(10), shipping_cost(2))
})
```

The test confirms that a heavy package costs more than a light one. It
passes inputs of 10 and 2 kg — both clearly on opposite sides of the
boundary — so it never exercises the value 5 itself.

**Mutation testing output:**

    ℹ Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   0 |   1 | 100 | > → <    | shipping.R 
    x |   1 |   1 |   0 |   0 |   2 |  50 | > → >=   | shipping.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 1 | SURVIVED 1 | NO COVERAGE 0 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

`> → >=` survives. Changing `weight_kg > 5` to `weight_kg >= 5` only
affects input `weight_kg = 5` exactly. For inputs 10 and 2, the function
returns identical results under both operators, so the test cannot tell
the operators apart.

In production, `>= 5` instead of `> 5` would route 5 kg shipments to the
expensive tier and no test would catch the regression.

#### The fix

``` r

test_that("heavy packages cost more than light ones", {
  expect_gt(shipping_cost(10), shipping_cost(2))
})

test_that("5kg falls into the lower-cost tier", {
  expect_equal(shipping_cost(5), 5.00)
})
```

Add a test that passes the exact boundary value `5`. With `> 5`, the
condition is `FALSE` and the function returns `5.00`. With `>= 5`, it is
`TRUE` and returns `15.00`. This difference kills the mutant.

**After the fix:**

    ℹ Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   0 |   1 | 100 | > → <    | shipping.R 
    ✔ |   2 |   0 |   0 |   0 |   2 | 100 | > → >=   | shipping.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 2 | SURVIVED 0 | NO COVERAGE 0 | ERRORS 0 | TOTAL 2 | SCORE 100.0% ]

> When a comparison mutant survives, find the boundary value implied by
> the operator and add a test that passes exactly that value.

------------------------------------------------------------------------

## Summary: three patterns, three fixes

| Surviving mutant | Root cause | Fix |
|----|----|----|
| Comparison (`>=` → `>`) | Boundary value never tested | Add a test that passes the exact threshold value |
| Arithmetic (`-` → `+`) | Assertion checks direction, not value | Replace `expect_gt/gte` with `expect_equal` and a computed expected value |
| Logical (`\|\|` → `&&`) | Symmetric inputs (both true or both false) | Add a test with asymmetric inputs (one true, one false) |

The feedback loop is:

1.  Run muttest
2.  Find survivors
3.  Strengthen tests
4.  Repeat

Start with one file and one mutator preset. Look at what survives. The
survivor names the exact mutant that your tests cannot distinguish —
that is your actionable signal.
