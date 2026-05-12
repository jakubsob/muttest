# Access Control — Symmetric Inputs Hide `||` vs `&&`

## What this demonstrates

When test inputs are symmetric — both arguments true, or both arguments false — `||` and `&&` produce identical results. A test suite built entirely from symmetric inputs cannot distinguish the two operators, so `|| → &&` survives. Adding a test with one argument true and the other false reveals the difference and kills the mutant.

## The function

`can_access` grants access when either `is_admin` or `is_owner` is true, using `||`. Swapping to `&&` would require *both* flags to be true — a fundamentally different access policy.

## The weak test

The tests cover only two cases: both `TRUE` (should allow access) and both `FALSE` (should deny access). These are the "happy path" and the "all-invalid" case — typical of tests written for simple scenarios without thinking about individual flag variation.

## The surviving mutant

`|| → &&` survives for both test cases:

- `can_access(TRUE, TRUE)`: `TRUE && TRUE = TRUE` — still passes `expect_true`
- `can_access(FALSE, FALSE)`: `FALSE && FALSE = FALSE` — still passes `expect_false`

The logical distinction between `||` and `&&` only appears when the operands *disagree*. No test ever passes `(FALSE, TRUE)` or `(TRUE, FALSE)`, so the function behaves identically for both operators across the entire test suite.

## The fix

Add tests with asymmetric inputs — one flag true and the other false. `can_access(FALSE, TRUE)` returns `TRUE` with `||` but `FALSE` with `&&`. This difference kills the mutant.

## Key rule

> When a logical operator mutant survives, add tests with asymmetric inputs (one flag true, the other false). These inputs reveal the difference between `&&` (requires both) and `||` (requires either).
