# Mean Absolute Deviation — Direction-Insensitive Assertion

## What this demonstrates

An assertion that checks a *property* of the result (sign, order, non-negativity) rather than the *value* will be blind to arithmetic operator swaps. Both the original and the mutant satisfy the same directional constraint, so the mutant survives. Replacing the directional assertion with an exact-value assertion kills it.

## The function

`mean_absolute_deviation` computes the average distance from a center point: `mean(abs(x - center))`. The subtraction `x - center` is the step that arithmetic operator mutators target.

## The weak test

The test asserts that the result is non-negative (`expect_gte(..., 0)`). This checks a property of the output rather than its value.

## The surviving mutant

`- → +` survives. Changing `x - center` to `x + center` gives a different number, but `abs(x + center)` is also non-negative for all inputs. For `x = c(1,3,5)` and `center = 3`:

- Original: `abs(c(-2, 0, 2))` → mean = 4/3 ≈ 1.33
- Mutant: `abs(c(4, 6, 8))` → mean = 6

Both are ≥ 0. The assertion cannot distinguish them.

This pattern is common in LLM-generated tests that check what is *obviously true* about the result (it is positive, it is numeric) rather than what is *specifically correct*.

## The fix

Replace the directional assertion with `expect_equal` and a value computed by hand. Now the mutant returns 6, which is not 4/3, and the test fails.

## Key rule

> When an arithmetic mutant survives, replace directional assertions (`expect_gt`, `expect_gte`) with exact-value assertions (`expect_equal`). Compute the expected value by hand and hard-code it.
