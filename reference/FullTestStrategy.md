# Run all tests for a mutant

This test strategy tells if a mutant is caught by any test.

To get faster results, especially for big codebases, use
[`?FileTestStrategy`](https://jakubsob.github.io/muttest/reference/FileTestStrategy.md)
instead.

## See also

Other TestStrategy:
[`FileTestStrategy`](https://jakubsob.github.io/muttest/reference/FileTestStrategy.md),
[`TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md),
[`default_test_strategy()`](https://jakubsob.github.io/muttest/reference/default_test_strategy.md)

## Super class

[`muttest::TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md)
-\> `FullTestStrategy`

## Methods

### Public methods

- [`FullTestStrategy$new()`](#method-FullTestStrategy-new)

- [`FullTestStrategy$execute()`](#method-FullTestStrategy-execute)

- [`FullTestStrategy$clone()`](#method-FullTestStrategy-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize

#### Usage

    FullTestStrategy$new(
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

    FullTestStrategy$execute(path, plan, reporter)

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

    FullTestStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
