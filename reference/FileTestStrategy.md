# Run tests matching the mutated source file name

This strategy tells if a mutant is caught by a test matching the source
file name.

For example, if the source file name is `foo.R`, and there are test
files named `test-foo.R` or `test-bar.R`, only `test-foo.R` will be run.

This strategy should give faster results than
[`?FullTestStrategy`](https://jakubsob.github.io/muttest/reference/FullTestStrategy.md),
especially for big codebases, but the score might be less accurate.

## See also

Other TestStrategy:
[`FullTestStrategy`](https://jakubsob.github.io/muttest/reference/FullTestStrategy.md),
[`TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md),
[`default_test_strategy()`](https://jakubsob.github.io/muttest/reference/default_test_strategy.md)

## Super class

[`muttest::TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md)
-\> `FileTestStrategy`

## Methods

### Public methods

- [`FileTestStrategy$new()`](#method-FileTestStrategy-new)

- [`FileTestStrategy$execute()`](#method-FileTestStrategy-execute)

- [`FileTestStrategy$clone()`](#method-FileTestStrategy-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the FileTestStrategy

#### Usage

    FileTestStrategy$new(
      load_helpers = TRUE,
      load_package = c("source", "none", "installed")
    )

#### Arguments

- `load_helpers`:

  Whether to load test helpers

- `load_package`:

  The package loading strategy

------------------------------------------------------------------------

### Method `execute()`

Execute the test strategy

#### Usage

    FileTestStrategy$execute(path, plan, reporter)

#### Arguments

- `path`:

  The path to the test directory

- `plan`:

  The current mutation plan. See
  [`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md).

- `reporter`:

  The reporter to use for test results

#### Returns

The test results

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FileTestStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
