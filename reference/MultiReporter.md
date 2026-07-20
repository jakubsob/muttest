# Combine Several Mutation Reporters

Fans out every reporter event to a list of child reporters, so a single
[`muttest()`](https://jakubsob.github.io/muttest/reference/muttest.md)
run can drive more than one reporter at once – for example a live
[ProgressMutationReporter](https://jakubsob.github.io/muttest/reference/ProgressMutationReporter.md)
alongside a
[JSONMutationReporter](https://jakubsob.github.io/muttest/reference/JSONMutationReporter.md)
that writes a machine-readable artifact.

## See also

Other MutationReporter:
[`JSONMutationReporter`](https://jakubsob.github.io/muttest/reference/JSONMutationReporter.md),
[`MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md),
[`ProgressMutationReporter`](https://jakubsob.github.io/muttest/reference/ProgressMutationReporter.md),
[`default_reporter()`](https://jakubsob.github.io/muttest/reference/default_reporter.md)

## Super class

[`muttest::MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md)
-\> `MultiReporter`

## Public fields

- `reporters`:

  List of child reporters.

## Methods

### Public methods

- [`MultiReporter$new()`](#method-MultiReporter-new)

- [`MultiReporter$start_reporter()`](#method-MultiReporter-start_reporter)

- [`MultiReporter$start_file()`](#method-MultiReporter-start_file)

- [`MultiReporter$start_mutator()`](#method-MultiReporter-start_mutator)

- [`MultiReporter$add_result()`](#method-MultiReporter-add_result)

- [`MultiReporter$update()`](#method-MultiReporter-update)

- [`MultiReporter$end_mutator()`](#method-MultiReporter-end_mutator)

- [`MultiReporter$end_file()`](#method-MultiReporter-end_file)

- [`MultiReporter$end_reporter()`](#method-MultiReporter-end_reporter)

- [`MultiReporter$get_score()`](#method-MultiReporter-get_score)

- [`MultiReporter$print()`](#method-MultiReporter-print)

- [`MultiReporter$clone()`](#method-MultiReporter-clone)

Inherited methods

- [`muttest::MutationReporter$cat_line()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-cat_line)
- [`muttest::MutationReporter$rule()`](https://jakubsob.github.io/muttest/reference/MutationReporter.html#method-rule)

------------------------------------------------------------------------

### Method `new()`

Initialize a combined reporter

#### Usage

    MultiReporter$new(...)

#### Arguments

- `...`:

  One or more
  [MutationReporter](https://jakubsob.github.io/muttest/reference/MutationReporter.md)
  objects.

------------------------------------------------------------------------

### Method `start_reporter()`

Start reporter

#### Usage

    MultiReporter$start_reporter(plan = NULL)

#### Arguments

- `plan`:

  The complete mutation plan

------------------------------------------------------------------------

### Method `start_file()`

Start testing a file

#### Usage

    MultiReporter$start_file(filename)

#### Arguments

- `filename`:

  Path to the file being mutated

------------------------------------------------------------------------

### Method `start_mutator()`

Start testing with a specific mutator

#### Usage

    MultiReporter$start_mutator(mutator)

#### Arguments

- `mutator`:

  The mutator being applied

------------------------------------------------------------------------

### Method `add_result()`

Add a mutation test result

#### Usage

    MultiReporter$add_result(...)

#### Arguments

- `...`:

  Arguments forwarded to each child's `add_result()`.

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update status

#### Usage

    MultiReporter$update(force = FALSE)

#### Arguments

- `force`:

  Passed to each child

------------------------------------------------------------------------

### Method `end_mutator()`

End testing with current mutator

#### Usage

    MultiReporter$end_mutator()

------------------------------------------------------------------------

### Method `end_file()`

End testing current file

#### Usage

    MultiReporter$end_file()

------------------------------------------------------------------------

### Method `end_reporter()`

End reporter

#### Usage

    MultiReporter$end_reporter()

------------------------------------------------------------------------

### Method `get_score()`

Get the score (from the first child)

#### Usage

    MultiReporter$get_score()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print each child that supports printing

#### Usage

    MultiReporter$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MultiReporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
