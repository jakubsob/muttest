# Mutator Reference and Choosing the Right Mutators

A mutator defines one kind of code change. When you pass a mutator to
[`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md),
muttest finds every matching pattern in your source file and produces
one mutant per match.

## Operator mutators

### `operator()` — custom pair

The most flexible mutator. Replaces any token with any other token.

``` r

operator("+", "-")   # every + becomes -
operator("==", "!=") # every == becomes !=
```

Use this when you need a specific swap not covered by the preset
functions.

### `arithmetic_operators()` — preset for arithmetic

Returns a ready-made list of operator mutators covering common
arithmetic mistakes:

| Original | Mutant |
|----------|--------|
| `+`      | `-`    |
| `-`      | `+`    |
| `*`      | `/`    |
| `/`      | `*`    |
| `^`      | `*`    |
| `%%`     | `*`    |
| `%/%`    | `/`    |

💡 **When to use:** Any function that performs calculations — finance,
statistics, data transformations. Arithmetic operator swaps can happen
easily and go unnoticed.

``` r

plan <- muttest_plan(
  source_files = "R/finance.R",
  mutators = arithmetic_operators()
)
```

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
      |   K |   S |   E |   T |   % | Mutator  | File 
    x |   0 |   1 |   0 |   1 |   0 | - → +    | mad.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 0 | SURVIVED 1 | ERRORS 0 | TOTAL 1 | SCORE 0.0% ]

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
      |   K |   S |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   1 | 100 | - → +    | mad.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 1 | SURVIVED 0 | ERRORS 0 | TOTAL 1 | SCORE 100.0% ]

> When an arithmetic mutant survives, replace directional assertions
> (`expect_gt`, `expect_gte`) with exact-value assertions
> (`expect_equal`). Compute the expected value by hand and hard-code it.

### `comparison_operators()` — preset for comparisons

Covers boundary and direction mistakes in comparison expressions:

| Original | Mutant |
|----------|--------|
| `<`      | `>`    |
| `>`      | `<`    |
| `<=`     | `>=`   |
| `>=`     | `<=`   |
| `==`     | `!=`   |
| `!=`     | `==`   |
| `<`      | `<=`   |
| `>`      | `>=`   |

💡 **When to use:** Functions with threshold logic, range checks, or
filter conditions. Off-by-one and direction errors are easy to introduce
and hard to catch with weak tests.

### Example: Shipping Cost — Missing Boundary Value

A test suite that checks only “clearly above” and “clearly below” cases
will let a boundary-shift mutant survive. The surviving mutant names the
exact input your tests have never exercised. Adding a test at that
precise boundary value kills it.

#### The function

``` r

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
      |   K |   S |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   1 | 100 | > → <    | shipping.R 
    x |   1 |   1 |   0 |   2 |  50 | > → >=   | shipping.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

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
      |   K |   S |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   1 | 100 | > → <    | shipping.R 
    ✔ |   2 |   0 |   0 |   2 | 100 | > → >=   | shipping.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 2 | SURVIVED 0 | ERRORS 0 | TOTAL 2 | SCORE 100.0% ]

> When a comparison mutant survives, find the boundary value implied by
> the operator and add a test that passes exactly that value.

### `logical_operators()` — preset for logical operators

| Original | Mutant |
|----------|--------|
| `&&`     | `\|\|` |
| `\|\|`   | `&&`   |
| `&`      | `\|`   |
| `\|`     | `&`    |

💡 **When to use:** Functions with compound conditions (`if (a && b)`).
Swapping `&&` for `||` is a classic logic bug that coverage cannot
detect.

### Example: Access Control — Symmetric Inputs Hide `||` vs `&&`

When test inputs are symmetric — both arguments true, or both arguments
false — `||` and `&&` produce identical results. A test suite built
entirely from symmetric inputs cannot distinguish the two operators, so
`|| → &&` survives. Adding a test with one argument true and the other
false reveals the difference and kills the mutant.

#### The function

``` r

can_access <- function(is_admin, is_owner) {
  is_admin || is_owner
}
```

`can_access` grants access when either `is_admin` or `is_owner` is true,
using `||`. Swapping to `&&` would require *both* flags to be true — a
fundamentally different access policy.

#### Weak test

``` r

test_that("access control works", {
  expect_true(can_access(TRUE, TRUE))
  expect_false(can_access(FALSE, FALSE))
})
```

The tests cover only two cases: both `TRUE` (should allow access) and
both `FALSE` (should deny access). These are the “happy path” and the
“all-invalid” case — typical of tests written for simple scenarios
without thinking about individual flag variation.

**Mutation testing output:**

    ℹ Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File 
    x |   0 |   1 |   0 |   1 |   0 | || → &&  | access.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 0 | SURVIVED 1 | ERRORS 0 | TOTAL 1 | SCORE 0.0% ]

`|| → &&` survives for both test cases:

- `can_access(TRUE, TRUE)`: `TRUE && TRUE = TRUE` — still passes
  `expect_true`
- `can_access(FALSE, FALSE)`: `FALSE && FALSE = FALSE` — still passes
  `expect_false`

The logical distinction between `||` and `&&` only appears when the
operands *disagree*. No test ever passes `(FALSE, TRUE)` or
`(TRUE, FALSE)`, so the function behaves identically for both operators
across the entire test suite.

#### The fix

``` r

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
```

Add tests with asymmetric inputs — one flag true and the other false.
`can_access(FALSE, TRUE)` returns `TRUE` with `||` but `FALSE` with
`&&`. This difference kills the mutant.

**After the fix:**

    ℹ Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File 
    ✔ |   1 |   0 |   0 |   1 | 100 | || → &&  | access.R 


    ── Results ─────────────────────────────────────────────────────────────────────
    [ KILLED 1 | SURVIVED 0 | ERRORS 0 | TOTAL 1 | SCORE 100.0% ]

> When a logical operator mutant survives, add tests with asymmetric
> inputs (one flag true, the other false). These inputs reveal the
> difference between `&&` (requires both) and `||` (requires either).

------------------------------------------------------------------------

## Boolean literal mutators

### `boolean_literal()` — flip TRUE/FALSE

``` r

boolean_literal("TRUE", "FALSE")  # TRUE → FALSE
boolean_literal("FALSE", "TRUE")  # FALSE → TRUE
```

💡 **When to use:** Functions with hard-coded boolean flags or default
arguments like `na.rm = TRUE`, `stringsAsFactors = FALSE`. Flipping the
default reveals whether tests exercise both states.

### `boolean_literals()` — preset for boolean literals

Returns a ready-made list covering all canonical flips:

| Original | Mutant  |
|----------|---------|
| `TRUE`   | `FALSE` |
| `FALSE`  | `TRUE`  |
| `T`      | `F`     |
| `F`      | `T`     |

``` r

plan <- muttest_plan(
  source_files = "R/flags.R",
  mutators = boolean_literals()
)
```

------------------------------------------------------------------------

## Numeric mutators

### `numeric_increment()` — add 1 to every numeric literal

``` r

numeric_increment()  # 5 → 6, 0 → 1, 100 → 101
```

### `numeric_decrement()` — subtract 1 from every numeric literal

``` r

numeric_decrement()  # 5 → 4, 1 → 0, 100 → 99
```

💡 **When to use:** Functions where exact numeric constants matter —
thresholds, window sizes, counts, index boundaries. Off-by-one errors in
constants are common and often untested.

### `numeric_literals()` — preset for numeric constants

Returns
[`numeric_increment()`](https://jakubsob.github.io/muttest/reference/numeric_increment.md)
and
[`numeric_decrement()`](https://jakubsob.github.io/muttest/reference/numeric_decrement.md)
together.

``` r

plan <- muttest_plan(
  source_files = "R/thresholds.R",
  mutators = numeric_literals()
)
```

------------------------------------------------------------------------

## String mutators

### `string_empty()` — replace strings with `""`

``` r

string_empty()  # "hello" → ""
```

### `string_fill()` — replace empty strings with `"mutant"`

``` r

string_fill()  # "" → "mutant"
```

💡 **When to use:**
[`string_empty()`](https://jakubsob.github.io/muttest/reference/string_empty.md)
is useful for functions that return or compare string values — it checks
whether your tests would notice a blank output.
[`string_fill()`](https://jakubsob.github.io/muttest/reference/string_fill.md)
tests whether your code handles non-empty strings where empty ones are
expected.

### `string_literals()` — preset for string literals

Returns
[`string_empty()`](https://jakubsob.github.io/muttest/reference/string_empty.md)
and
[`string_fill()`](https://jakubsob.github.io/muttest/reference/string_fill.md)
together.

``` r

plan <- muttest_plan(
  source_files = "R/labels.R",
  mutators = string_literals()
)
```

------------------------------------------------------------------------

## Condition mutators

### `negate_condition()` — wrap condition in `!(...)`

``` r

negate_condition()
# if (x > 0)   →   if (!(x > 0))
# while (done) →   while (!(done))
```

### `remove_condition_negation()` — strip leading `!`

``` r

remove_condition_negation()
# if (!done)  →  if (done)
# if (!valid) →  if (valid)
```

### `remove_negation()` — remove `!` anywhere

``` r

remove_negation()
# !is.na(x)    →  is.na(x)
# !is.null(y)  →  is.null(y)
```

💡 **When to use:** Functions with guard clauses and early returns.
These mutators reveal whether your tests cover both branches of a
condition. A surviving
[`negate_condition()`](https://jakubsob.github.io/muttest/reference/negate_condition.md)
mutant means the test inputs never trigger the `FALSE` branch.

### `condition_mutations()` — preset for condition logic

Returns
[`negate_condition()`](https://jakubsob.github.io/muttest/reference/negate_condition.md)
and
[`remove_condition_negation()`](https://jakubsob.github.io/muttest/reference/remove_condition_negation.md)
together.

``` r

plan <- muttest_plan(
  source_files = "R/validation.R",
  mutators = condition_mutations()
)
```

------------------------------------------------------------------------

## Function call mutator

### `call_name()` — swap one function name for another

``` r

call_name("any", "all")   # any(x) → all(x)
call_name("min", "max")   # min(x) → max(x)
call_name("sum", "prod")  # sum(x) → prod(x)
```

💡 **When to use:** Functions that delegate to summary or aggregation
helpers. `any` vs `all` and `min` vs `max` are among the easiest
mistakes to make and the hardest to spot in a review.

------------------------------------------------------------------------

------------------------------------------------------------------------

## NA and NULL mutators

### `na_literal()` — swap NA/NULL values

``` r

na_literal("NA", "NULL")          # NA → NULL
na_literal("NULL", "NA")          # NULL → NA
na_literal("NA", "NA_real_")      # NA → NA_real_
na_literal("NA_real_", "NA")      # NA_real_ → NA
```

💡 **When to use:** Functions that accept or return `NA` or `NULL`,
especially any code with [`is.na()`](https://rdrr.io/r/base/NA.html),
[`is.null()`](https://rdrr.io/r/base/NULL.html), or `na.rm` handling.
Swapping `NA` for `NULL` (and vice versa) reveals whether callers
distinguish between “value is missing” and “value is absent” — two
distinct concepts that R treats very differently.

Swapping between typed NAs (`NA_real_`, `NA_integer_`, `NA_character_`)
and plain `NA` checks whether type-sensitive downstream code
(e.g. `vapply`,
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)) is
covered.

### `na_literals()` — preset for NA and NULL

Returns mutators for all common NA/NULL swaps:

| Original        | Mutant          |
|-----------------|-----------------|
| `NA`            | `NULL`          |
| `NULL`          | `NA`            |
| `NA`            | `NA_real_`      |
| `NA_real_`      | `NA`            |
| `NA`            | `NA_integer_`   |
| `NA_integer_`   | `NA`            |
| `NA`            | `NA_character_` |
| `NA_character_` | `NA`            |

``` r

plan <- muttest_plan(
  source_files = "R/missing.R",
  mutators = na_literals()
)
```

------------------------------------------------------------------------

## Return value mutators

### `replace_return_value()` — replace explicit return values

``` r

replace_return_value()       # return(x) → return(NULL)
replace_return_value("NA")   # return(x) → return(NA)
```

💡 **When to use:** Any function with explicit
[`return()`](https://rdrr.io/r/base/function.html) calls. Tests that
only check that a function *runs without error* or *returns something*
will not kill these mutants — only tests that assert the specific value
returned will.

A surviving
[`replace_return_value()`](https://jakubsob.github.io/muttest/reference/replace_return_value.md)
mutant means the caller never checks what came back from that branch.
This is especially common in functions with multiple early-exit
[`return()`](https://rdrr.io/r/base/function.html) paths where only the
happy path is tested.

Only explicit `return(expr)` calls are targeted; implicit returns (the
last expression of a function body) are not affected.

------------------------------------------------------------------------

## Index and subscript mutators

### `index_increment()` — shift subscript indices up by one

``` r

index_increment()   # x[i] → x[i + 1L],  x[[i]] → x[[i + 1L]]
```

### `index_decrement()` — shift subscript indices down by one

``` r

index_decrement()   # x[i] → x[i - 1L],  x[[i]] → x[[i - 1L]]
```

💡 **When to use:** Functions that index into vectors or lists by
position or by a computed variable. Off-by-one errors in indexing are
among the most common silent bugs in R — they produce a different
element rather than an error, so they pass all tests unless a specific
element value is asserted.

Only simple indices are mutated: plain identifiers (`x[i]`) and numeric
literals (`x[1]`, `x[1L]`). Complex expressions like `x[a + b]` or
`x[seq_len(n)]` are left untouched, keeping the mutant count focused and
the signal-to-noise ratio high.

Both single-bracket (`[`) and double-bracket (`[[`) indexing are
covered.

### `index_mutations()` — preset for subscript indices

Returns
[`index_increment()`](https://jakubsob.github.io/muttest/reference/index_increment.md)
and
[`index_decrement()`](https://jakubsob.github.io/muttest/reference/index_decrement.md)
together.

``` r

plan <- muttest_plan(
  source_files = "R/selectors.R",
  mutators = index_mutations()
)
```

------------------------------------------------------------------------

## Recommended starting set

If you are unsure where to begin, start here:

``` r

p <- muttest_plan(
  source_files = "R/my_file.R",
  mutators = c(
    arithmetic_operators(),
    comparison_operators(),
    logical_operators(),
    condition_mutations(),
    numeric_literals(),
    list(remove_negation())
  )
)
```

Look at the survivors, then layer in
[`boolean_literals()`](https://jakubsob.github.io/muttest/reference/boolean_literals.md),
[`na_literals()`](https://jakubsob.github.io/muttest/reference/na_literals.md),
[`string_literals()`](https://jakubsob.github.io/muttest/reference/string_literals.md),
or
[`index_mutations()`](https://jakubsob.github.io/muttest/reference/index_mutations.md)
for the specific patterns in your code.

The better you know your codebase, the more you can target mutators to
the patterns that are most likely to harbor bugs. For example, if your
code has a lot of `if` statements but no numeric constants, condition
mutators will be more effective than numeric ones.

You’ll figure it out as you go.
