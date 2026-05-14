# Create a plan for mutation testing

Each mutant requires rerunning the tests. For large project it might be
not feasible to test all mutants in one go. This function allows you to
create a plan for selected source files and mutators.

## Usage

``` r
muttest_plan(mutators, source_files = fs::dir_ls("R", regexp = ".[rR]$"))
```

## Arguments

- mutators:

  A list of mutators to use. See
  [`operator()`](https://jakubsob.github.io/muttest/reference/operator.md).

- source_files:

  A vector of file paths to the source files.

## Value

A data frame with the test plan. The data frame has the following
columns:

- `filename`: The name of the source file.

- `original_code`: The original code of the source file.

- `mutated_code`: The mutated code of the source file.

- `mutator`: The mutator that was applied.

## Details

The plan is in a data frame format, where each row represents a mutant.

You can subset the plan before passing it to the
[`muttest()`](https://jakubsob.github.io/muttest/reference/muttest.md)
function.
