# Progress Reporter for Mutation Testing

A reporter that displays a progress indicator for mutation tests. It
provides real-time feedback on which mutants are being tested and
whether they were killed by tests.

## See also

Other MutationReporter:
[`MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md),
[`default_reporter()`](https://jakubsob.github.io/muttest/reference/default_reporter.md)

## Super class

[`muttest::MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md)
-\> `ProgressMutationReporter`

## Public fields

- `start_time`:

  Time when testing started (for duration calculation)

- `min_time`:

  Minimum test duration to display timing information

- `col_config`:

  List of column configuration for report formatting

- `survived_detail`:

  Controls how survived mutants are reported (summary, none)

- `survived_mutants`:

  List to store details of survived mutants for summary reporting

## Methods

### Public methods

- [`ProgressMutationReporter$format_column()`](#method-ProgressMutationReporter-format_column)

- [`ProgressMutationReporter$fmt_h()`](#method-ProgressMutationReporter-fmt_h)

- [`ProgressMutationReporter$fmt_r()`](#method-ProgressMutationReporter-fmt_r)

- [`ProgressMutationReporter$new()`](#method-ProgressMutationReporter-new)

- [`ProgressMutationReporter$start_reporter()`](#method-ProgressMutationReporter-start_reporter)

- [`ProgressMutationReporter$add_result()`](#method-ProgressMutationReporter-add_result)

- [`ProgressMutationReporter$update()`](#method-ProgressMutationReporter-update)

- [`ProgressMutationReporter$end_file()`](#method-ProgressMutationReporter-end_file)

- [`ProgressMutationReporter$cr()`](#method-ProgressMutationReporter-cr)

- [`ProgressMutationReporter$end_reporter()`](#method-ProgressMutationReporter-end_reporter)

- [`ProgressMutationReporter$print()`](#method-ProgressMutationReporter-print)

- [`ProgressMutationReporter$clone()`](#method-ProgressMutationReporter-clone)

Inherited methods

- [`muttest::MutationReporter$cat_line()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-cat_line)
- [`muttest::MutationReporter$cat_tight()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-cat_tight)
- [`muttest::MutationReporter$end_mutator()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-end_mutator)
- [`muttest::MutationReporter$get_score()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-get_score)
- [`muttest::MutationReporter$rule()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-rule)
- [`muttest::MutationReporter$start_file()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-start_file)
- [`muttest::MutationReporter$start_mutator()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-start_mutator)

------------------------------------------------------------------------

### Method `format_column()`

Format a column with specified padding and width

#### Usage

    ProgressMutationReporter$format_column(text, col_name, colorize = NULL)

#### Arguments

- `text`:

  Text to format

- `col_name`:

  Column name to use configuration from

- `colorize`:

  Optional function to color the text

------------------------------------------------------------------------

### Method `fmt_h()`

Format the header of the report

#### Usage

    ProgressMutationReporter$fmt_h()

------------------------------------------------------------------------

### Method `fmt_r()`

Format a row of the report

#### Usage

    ProgressMutationReporter$fmt_r(status, k, s, e, t, score, mutator, file)

#### Arguments

- `status`:

  Status symbol (e.g., tick or cross)

- `k`:

  Number of killed mutations

- `s`:

  Number of survived mutations

- `e`:

  Number of errors

- `t`:

  Total number of mutations

- `score`:

  Score percentage

- `mutator`:

  The mutator used

- `file`:

  The file being tested

#### Returns

Formatted row string

------------------------------------------------------------------------

### Method `new()`

Initialize a new progress reporter

#### Usage

    ProgressMutationReporter$new(
      test_reporter = "silent",
      min_time = 1,
      file = stdout(),
      survived_detail = c("summary", "none")
    )

#### Arguments

- `test_reporter`:

  Reporter to use for testthat::test_dir

- `min_time`:

  Minimum time to show elapsed time (default: 1s)

- `file`:

  Output destination (default: stdout)

- `survived_detail`:

  Controls how survived mutants are reported. One of `"summary"`
  (default) or `"none"`.

------------------------------------------------------------------------

### Method `start_reporter()`

Start reporter

#### Usage

    ProgressMutationReporter$start_reporter(plan = NULL)

#### Arguments

- `plan`:

  The complete mutation plan

------------------------------------------------------------------------

### Method `add_result()`

Add a mutation test result

#### Usage

    ProgressMutationReporter$add_result(
      plan,
      killed,
      survived,
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

Update status spinner (for long-running operations)

#### Usage

    ProgressMutationReporter$update(force = FALSE)

#### Arguments

- `force`:

  Force update even if interval hasn't elapsed

------------------------------------------------------------------------

### Method `end_file()`

End testing current file

#### Usage

    ProgressMutationReporter$end_file()

------------------------------------------------------------------------

### Method `cr()`

Carriage return if dynamic, newline otherwise

#### Usage

    ProgressMutationReporter$cr()

------------------------------------------------------------------------

### Method `end_reporter()`

End reporter with detailed summary

#### Usage

    ProgressMutationReporter$end_reporter()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the mutation test result with survived diffs

#### Usage

    ProgressMutationReporter$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProgressMutationReporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
