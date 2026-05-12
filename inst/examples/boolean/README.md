# Guard — Type Check Instead of Value Check

## What this demonstrates

Assertions that check the *type* of a return value rather than its *content* cannot detect boolean literal mutations. A `TRUE → FALSE` swap changes the value the function returns but not its type. Replacing type assertions with value assertions kills the mutant.

## The function

`is_valid` returns `FALSE` for `NULL` and `TRUE` for everything else. `all_valid` returns `TRUE` only if every element in a list passes `is_valid`. Both functions contain boolean literals (`TRUE`, `FALSE`) that boolean literal mutators will flip.

## The weak test

The tests only verify that the return value is a logical (`is.logical(...)`). A flipped boolean is still a logical — the type check passes for both the original and the mutant.

## The surviving mutant

`FALSE → TRUE` survives. When `is_valid(NULL)` returns `TRUE` instead of `FALSE`, `is.logical(TRUE)` is still `TRUE`. The test cannot tell the difference.

## The fix

Replace type assertions with value assertions using `expect_true` and `expect_false`. These check the actual boolean value returned, not just its type.

## Key rule

> When a boolean literal mutant survives, replace `is.logical()` type checks with `expect_true()` / `expect_false()` value checks.
