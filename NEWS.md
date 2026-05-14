# muttest 0.2.0

- ❗️ Renamed `plan()` to `muttest_plan()`.
- ✨ Add new mutators:
  - `boolean_literal()`, `na_literal()` — literal replacement mutators.
  - `negate_condition()`, `remove_condition_negation()` — condition mutators.
  - `numeric_increment()`, `numeric_decrement()`, `index_increment()`, `index_decrement()` — increment/decrement mutators.
  - `string_empty()`, `string_fill()` — string mutators.
  - `call_name()` — call name mutator.
  - `remove_negation()` — unary negation mutator.
  - `replace_return_value()` — return value mutator.
  - `delete_statement()` — removes assignments and standalone calls one at a time to catch untested side effects and dead assignments.
- ✨ Add preset collections for all major mutator groups:
  - `boolean_literals()` — flips `TRUE`/`FALSE` and `T`/`F`.
  - `na_literals()` — swaps `NA`, `NULL`, and typed NA variants.
  - `numeric_literals()` — increments and decrements numeric constants.
  - `index_mutations()` — shifts subscript indices up and down by one.
  - `string_literals()` — empties non-empty strings and fills empty ones.
  - `condition_mutations()` — negates conditions and removes existing negations.
  - `arithmetic_operators()` — mutates arithmetic operators (`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`).
  - `comparison_operators()` — mutates comparison operators (`<`, `>`, `<=`, `>=`, `==`, `!=`).
  - `logical_operators()` — mutates logical operators (`&`, `|`, `&&`, `||`).
- ✨ Mutators are now parametrized and accept configuration arguments.
- ✨ `ProgressMutationReporter` now reports survived mutants.
- ✨ Expose `Mutator` base class for building custom mutators.
- ✨ `muttest()` supports parallel execution of mutants.
- ✨ `muttest()` supports per-mutant timeouts to avoid hanging test runs.
- 🥅 Improved error handling when test execution fails.

# muttest 0.1.0

- ✨ Create a testing plan with `plan`.
- ✨ Run mutation testing with `muttest`.
- ✨ Support mutating operators with `operator`.
- ✨ Control copying project to temporary directory with `CopyStrategy`:
  - `PackageCopyStrategy` implemented for copying package files.
- ✨ Control test execution for each mutant with `TestStrategy`:
  - `FullTestStrategy` for running all tests for each mutant.
  - `FileTestStrategy` for running only test files matching mutant files.
- ✨ See test results with `MutationReporter`.
  - `ProgressMutationReporter` for printing progress to the console.
