# Run a mutation test

Run a mutation test

## Usage

``` r
muttest(
  plan,
  path = "tests/testthat",
  reporter = default_reporter(),
  test_strategy = default_test_strategy(),
  copy_strategy = default_copy_strategy(),
  workers = 1,
  timeout = 600 * 1000
)
```

## Arguments

- plan:

  A mutation testing plan. See
  [`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md).

- path:

  Path to the test directory.

- reporter:

  Reporter to use for mutation testing results. See
  [`?MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md).

- test_strategy:

  Strategy for running tests. See
  [`?TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md).
  The purpose of test strategy is to control how tests are executed. We
  can run all tests for each mutant, or only tests that are relevant to
  the mutant.

- copy_strategy:

  Strategy for copying the project. See
  [`?CopyStrategy`](https://jakubsob.github.io/muttest/reference/CopyStrategy.md).
  This strategy controls which files are copied to the temporary
  directory, where the tests are run.

- workers:

  Number of parallel workers. When greater than 1, mutants are tested
  concurrently using `mirai` daemons. Defaults to 1 (sequential).

- timeout:

  Per-mutant timeout in milliseconds. If a mutant's test run exceeds
  this limit the daemon is interrupted and the result is recorded as an
  error. Use `Inf` to disable. Defaults to 600000 (10 minutes).

## Value

An object of class `muttest_result` containing the overall mutation
score.
