# Classify — Single-Branch Testing

## What this demonstrates

When tests only exercise one branch of a multi-branch condition, negating the condition can redirect execution to a different branch while still producing a result that passes the assertion. Testing all branches and asserting exact values kills condition-negation mutants.

## The function

`sign_of` classifies a number as `"positive"`, `"negative"`, or `"zero"` using two chained conditions. `clamp` constrains a value to a range using two guards. Each `if` condition is a target for `negate_condition()`.

## The weak test

The tests check only that `sign_of(5)` returns a string (type check) and that `clamp(7, 5, 10)` returns a number (type check). A negated condition may take a different branch but still return a string or a number.

## The surviving mutant

`negate_condition()` on `if (x > 0)` produces `if (!(x > 0))`. For `x = 5`:
- Original: `!(5 > 0)` = `FALSE` → falls through to `else if (x < 0)` which is also `FALSE` → returns `"zero"`.
- Actually: original `if (5 > 0)` = `TRUE` → returns `"positive"`.
- Mutant: `if (!(5 > 0))` = `FALSE` → checks `else if (5 < 0)` = `FALSE` → returns `"zero"`.

`is.character("zero")` = `TRUE` — the type check passes for the mutated return value.

## The fix

Assert the exact string returned for each branch. Covering all three cases (`"positive"`, `"negative"`, `"zero"`) ensures that redirecting to a different branch fails the test.

## Key rule

> When a condition-negation mutant survives, add tests for every branch of the condition and assert the exact return value for each case.
