# Calculate — Type Check Instead of Value Check

## What this demonstrates

An assertion that only checks whether a result is numeric cannot detect arithmetic operator swaps. Different operators produce different numbers, but all of them are numeric. Asserting the exact expected value kills the mutant.

## The function

`calculate(x, y)` computes `x * y + x`. It contains both `*` and `+`, giving arithmetic operator mutators two targets to swap.

## The weak test

The test asserts only that the result of `calculate(2, 3)` is numeric. Any arithmetic expression on two numbers returns a number, so `is.numeric()` passes for the original and all mutants.

## The surviving mutant

`* → /` produces `x / y + x`. For `x = 2, y = 3`: `2/3 + 2 ≈ 2.67` — numeric. `+ → -` produces `x * y - x`. For the same inputs: `2*3 - 2 = 4` — also numeric. Both mutants satisfy `is.numeric()`.

## The fix

Assert the exact expected value. `calculate(2, 3)` should equal `2 * 3 + 2 = 8`. The mutants return `≈ 2.67` and `4` respectively — neither equals 8, so both are killed.

## Key rule

> When an arithmetic mutant survives, replace type or property checks with `expect_equal` and an exact numeric value computed by hand.
