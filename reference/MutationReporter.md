# Reporter for Mutation Testing

The job of a mutation reporter is to aggregate and display the results
of mutation tests. It tracks each mutation attempt, reporting on whether
the tests killed the mutation or the mutation survived.

## See also

Other MutationReporter:
[`JSONMutationReporter`](https://jakubsob.github.io/muttest/reference/JSONMutationReporter.md),
[`MultiReporter`](https://jakubsob.github.io/muttest/reference/MultiReporter.md),
[`ProgressMutationReporter`](https://jakubsob.github.io/muttest/reference/ProgressMutationReporter.md),
[`default_reporter()`](https://jakubsob.github.io/muttest/reference/default_reporter.md)

## Public fields

- `test_reporter`:

  Reporter to use for the testthat::test_dir function

- `out`:

  Output destination for reporter messages

- `width`:

  Width of the console in characters

- `unicode`:

  Whether Unicode output is supported

- `crayon`:

  Whether colored output is supported

- `rstudio`:

  Whether running in RStudio

- `hyperlinks`:

  Whether terminal hyperlinks are supported

- `current_file`:

  Path of the file currently being mutated

- `current_mutator`:

  Mutator currently being applied

- `plan`:

  Complete mutation plan for the test run

- `results`:

  List of mutation test results, indexed by file path

- `current_score`:

  Current score of the mutation tests

- `error_messages`:

  List of error messages from failed mutant runs

## Methods

### Public methods

- [`MutationReporter$new()`](#method-MutationReporter-new)

- [`MutationReporter$start_reporter()`](#method-MutationReporter-start_reporter)

- [`MutationReporter$start_file()`](#method-MutationReporter-start_file)

- [`MutationReporter$start_mutator()`](#method-MutationReporter-start_mutator)

- [`MutationReporter$add_result()`](#method-MutationReporter-add_result)

- [`MutationReporter$update()`](#method-MutationReporter-update)

- [`MutationReporter$end_mutator()`](#method-MutationReporter-end_mutator)

- [`MutationReporter$end_file()`](#method-MutationReporter-end_file)

- [`MutationReporter$end_reporter()`](#method-MutationReporter-end_reporter)

- [`MutationReporter$get_score()`](#method-MutationReporter-get_score)

- [`MutationReporter$cat_line()`](#method-MutationReporter-cat_line)

- [`MutationReporter$rule()`](#method-MutationReporter-rule)

- [`MutationReporter$clone()`](#method-MutationReporter-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new reporter

#### Usage

    MutationReporter$new(test_reporter = "silent", file = stdout())

#### Arguments

- `test_reporter`:

  Reporter to use for the testthat::test_dir function

- `file`:

  Output destination (default: stdout)

------------------------------------------------------------------------

### Method `start_reporter()`

Start reporter

#### Usage

    MutationReporter$start_reporter(plan = NULL)

#### Arguments

- `plan`:

  The complete mutation plan

- `temp_dir`:

  Path to the temporary directory for testing

------------------------------------------------------------------------

### Method `start_file()`

Start testing a file

#### Usage

    MutationReporter$start_file(filename)

#### Arguments

- `filename`:

  Path to the file being mutated

------------------------------------------------------------------------

### Method `start_mutator()`

Start testing with a specific mutator

#### Usage

    MutationReporter$start_mutator(mutator)

#### Arguments

- `mutator`:

  The mutator being applied

------------------------------------------------------------------------

### Method `add_result()`

Add a mutation test result

#### Usage

    MutationReporter$add_result(
      plan,
      killed,
      survived,
      no_coverage,
      errors,
      error = NULL,
      original_code = NULL,
      mutated_code = NULL
    )

#### Arguments

- `plan`:

  Current testing plan. See
  [`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md).

- `killed`:

  Whether the mutation was killed by tests

- `survived`:

  Number of survived mutations

- `no_coverage`:

  Number of mutants with no test coverage

- `errors`:

  Number of errors encountered

- `error`:

  Optional error condition from a failed run

- `original_code`:

  Original source lines before mutation

- `mutated_code`:

  Mutated source lines

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update status (no-op in base class)

#### Usage

    MutationReporter$update(force = FALSE)

#### Arguments

- `force`:

  Ignored

------------------------------------------------------------------------

### Method `end_mutator()`

End testing with current mutator

#### Usage

    MutationReporter$end_mutator()

------------------------------------------------------------------------

### Method `end_file()`

End testing current file

#### Usage

    MutationReporter$end_file()

------------------------------------------------------------------------

### Method `end_reporter()`

End reporter and show summary

#### Usage

    MutationReporter$end_reporter()

------------------------------------------------------------------------

### Method `get_score()`

Get the current score

#### Usage

    MutationReporter$get_score()

------------------------------------------------------------------------

### Method `cat_line()`

Print a message to the output

#### Usage

    MutationReporter$cat_line(...)

#### Arguments

- `...`:

  Message to print

------------------------------------------------------------------------

### Method `rule()`

Print a message to the output with a rule

#### Usage

    MutationReporter$rule(...)

#### Arguments

- `...`:

  Message to print

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MutationReporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
