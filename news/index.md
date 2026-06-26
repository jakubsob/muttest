# Changelog

## muttest 0.2.1

CRAN release: 2026-06-24

- Update treesitter queries to work with treesitter \>= 1.3.0.

## muttest 0.2.0

CRAN release: 2026-05-14

- ❗️ Renamed `plan()` to
  [`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md).
- ✨ Add new mutators:
  - [`boolean_literal()`](https://jakubsob.github.io/muttest/reference/boolean_literal.md),
    [`na_literal()`](https://jakubsob.github.io/muttest/reference/na_literal.md)
    — literal replacement mutators.
  - [`negate_condition()`](https://jakubsob.github.io/muttest/reference/negate_condition.md),
    [`remove_condition_negation()`](https://jakubsob.github.io/muttest/reference/remove_condition_negation.md)
    — condition mutators.
  - [`numeric_increment()`](https://jakubsob.github.io/muttest/reference/numeric_increment.md),
    [`numeric_decrement()`](https://jakubsob.github.io/muttest/reference/numeric_decrement.md),
    [`index_increment()`](https://jakubsob.github.io/muttest/reference/index_increment.md),
    [`index_decrement()`](https://jakubsob.github.io/muttest/reference/index_decrement.md)
    — increment/decrement mutators.
  - [`string_empty()`](https://jakubsob.github.io/muttest/reference/string_empty.md),
    [`string_fill()`](https://jakubsob.github.io/muttest/reference/string_fill.md)
    — string mutators.
  - [`call_name()`](https://jakubsob.github.io/muttest/reference/call_name.md)
    — call name mutator.
  - [`remove_negation()`](https://jakubsob.github.io/muttest/reference/remove_negation.md)
    — unary negation mutator.
  - [`replace_return_value()`](https://jakubsob.github.io/muttest/reference/replace_return_value.md)
    — return value mutator.
  - [`delete_statement()`](https://jakubsob.github.io/muttest/reference/delete_statement.md)
    — removes assignments and standalone calls one at a time to catch
    untested side effects and dead assignments.
- ✨ Add preset collections for all major mutator groups:
  - [`boolean_literals()`](https://jakubsob.github.io/muttest/reference/boolean_literals.md)
    — flips `TRUE`/`FALSE` and `T`/`F`.
  - [`na_literals()`](https://jakubsob.github.io/muttest/reference/na_literals.md)
    — swaps `NA`, `NULL`, and typed NA variants.
  - [`numeric_literals()`](https://jakubsob.github.io/muttest/reference/numeric_literals.md)
    — increments and decrements numeric constants.
  - [`index_mutations()`](https://jakubsob.github.io/muttest/reference/index_mutations.md)
    — shifts subscript indices up and down by one.
  - [`string_literals()`](https://jakubsob.github.io/muttest/reference/string_literals.md)
    — empties non-empty strings and fills empty ones.
  - [`condition_mutations()`](https://jakubsob.github.io/muttest/reference/condition_mutations.md)
    — negates conditions and removes existing negations.
  - [`arithmetic_operators()`](https://jakubsob.github.io/muttest/reference/arithmetic_operators.md)
    — mutates arithmetic operators (`+`, `-`, `*`, `/`, `^`, `%%`,
    `%/%`).
  - [`comparison_operators()`](https://jakubsob.github.io/muttest/reference/comparison_operators.md)
    — mutates comparison operators (`<`, `>`, `<=`, `>=`, `==`, `!=`).
  - [`logical_operators()`](https://jakubsob.github.io/muttest/reference/logical_operators.md)
    — mutates logical operators (`&`, `|`, `&&`, `||`).
- ✨ Mutators are now parametrized and accept configuration arguments.
- ✨ `ProgressMutationReporter` now reports survived mutants.
- ✨ Expose `Mutator` base class for building custom mutators.
- ✨
  [`muttest()`](https://jakubsob.github.io/muttest/reference/muttest.md)
  supports parallel execution of mutants.
- ✨
  [`muttest()`](https://jakubsob.github.io/muttest/reference/muttest.md)
  supports per-mutant timeouts to avoid hanging test runs.
- 🥅 Improved error handling when test execution fails.

## muttest 0.1.0

CRAN release: 2025-05-30

- ✨ Create a testing plan with `plan`.
- ✨ Run mutation testing with `muttest`.
- ✨ Support mutating operators with `operator`.
- ✨ Control copying project to temporary directory with `CopyStrategy`:
  - `PackageCopyStrategy` implemented for copying package files.
- ✨ Control test execution for each mutant with `TestStrategy`:
  - `FullTestStrategy` for running all tests for each mutant.
  - `FileTestStrategy` for running only test files matching mutant
    files.
- ✨ See test results with `MutationReporter`.
  - `ProgressMutationReporter` for printing progress to the console.
