# Aggregate — Symmetric Inputs Hide Function Swaps

## What this demonstrates

When test inputs are chosen so that two semantically different functions (`any`/`all`, `max`/`min`, `sum`/`prod`) return the same result, swapping one for the other produces no observable difference. The mutant survives. Choosing discriminating inputs — where the two functions return different values — kills the mutant.

## The function

`has_positive` uses `any`, `all_positive` uses `all`, `range_width` uses `max` and `min`, and `total` uses `sum`. Each of these function calls is a target for call-name mutation.

## The weak test

The tests use inputs where `any` and `all` agree (all-positive vectors), check only the sign of `range_width` (non-negative), and verify only the type of `total` (numeric). None of these assertions can detect the corresponding function swap.

## The surviving mutant

- `any → all` survives with all-positive input: `any(c(1,2,3) > 0)` = `TRUE`, `all(c(1,2,3) > 0)` = `TRUE` — same result.
- `max → min` survives: `min(c(1,5,3)) - min(c(1,5,3))` = 0, which is still ≥ 0.
- `sum → prod` survives: `prod(c(1,2,3))` = 6 = `sum(c(1,2,3))` — coincidentally equal.

## The fix

Use mixed inputs that produce different results for each function pair. For `any`/`all`, pass a vector with some positives and some non-positives. For `range_width`, assert the exact value. For `total`, use inputs where `sum` ≠ `prod`.

## Key rule

> When a call-name mutant survives, choose inputs that discriminate between the original function and its swap — the two functions must return different values for the test to be useful.
