# JSON Reporter for Mutation Testing

A quiet reporter that writes a machine-readable JSON artifact. It prints
nothing while running; on completion it writes one JSON file describing
every mutant, keyed by source file. The JSON is the data contract
consumed by dashboards, CI job summaries, and the HTML report (see
[`report()`](https://jakubsob.github.io/muttest/reference/report.md)).

Combine it with
[ProgressMutationReporter](https://jakubsob.github.io/muttest/reference/ProgressMutationReporter.md)
via
[MultiReporter](https://jakubsob.github.io/muttest/reference/MultiReporter.md)
to get a live console display and a JSON file from the same run.

## See also

Other MutationReporter:
[`MultiReporter`](https://jakubsob.github.io/muttest/reference/MultiReporter.md),
[`MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md),
[`ProgressMutationReporter`](https://jakubsob.github.io/muttest/reference/ProgressMutationReporter.md),
[`default_reporter()`](https://jakubsob.github.io/muttest/reference/default_reporter.md)

## Super class

[`muttest::MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md)
-\> `JSONMutationReporter`

## Public fields

- `path`:

  Path of the JSON file to write.

- `mutants_by_file`:

  Per-file lists of mutant records.

- `sources`:

  Per-file original source lines.

## Methods

### Public methods

- [`JSONMutationReporter$new()`](#method-JSONMutationReporter-new)

- [`JSONMutationReporter$start_reporter()`](#method-JSONMutationReporter-start_reporter)

- [`JSONMutationReporter$add_result()`](#method-JSONMutationReporter-add_result)

- [`JSONMutationReporter$end_reporter()`](#method-JSONMutationReporter-end_reporter)

- [`JSONMutationReporter$clone()`](#method-JSONMutationReporter-clone)

Inherited methods

- [`muttest::MutationReporter$cat_line()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-cat_line)
- [`muttest::MutationReporter$end_file()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-end_file)
- [`muttest::MutationReporter$end_mutator()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-end_mutator)
- [`muttest::MutationReporter$get_score()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-get_score)
- [`muttest::MutationReporter$rule()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-rule)
- [`muttest::MutationReporter$start_file()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-start_file)
- [`muttest::MutationReporter$start_mutator()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-start_mutator)
- [`muttest::MutationReporter$update()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-update)

------------------------------------------------------------------------

### Method `new()`

Initialize a new JSON reporter

#### Usage

    JSONMutationReporter$new(path = "muttest.json", ...)

#### Arguments

- `path`:

  Path of the JSON file to write (default: `"muttest.json"`).

- `...`:

  Passed to the
  [MutationReporter](https://jakubsob.github.io/muttest/reference/MutationReporter.md)
  constructor.

------------------------------------------------------------------------

### Method `start_reporter()`

Start reporter

#### Usage

    JSONMutationReporter$start_reporter(plan = NULL)

#### Arguments

- `plan`:

  The complete mutation plan

------------------------------------------------------------------------

### Method `add_result()`

Add a mutation test result

#### Usage

    JSONMutationReporter$add_result(
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

### Method `end_reporter()`

End reporter, then write the JSON file

#### Usage

    JSONMutationReporter$end_reporter()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    JSONMutationReporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
