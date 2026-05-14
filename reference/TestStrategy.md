# TestStrategy interface

Extend this class to implement a custom test strategy.

## See also

Other TestStrategy:
[`FileTestStrategy`](https://jakubsob.github.io/muttest/reference/FileTestStrategy.md),
[`FullTestStrategy`](https://jakubsob.github.io/muttest/reference/FullTestStrategy.md),
[`default_test_strategy()`](https://jakubsob.github.io/muttest/reference/default_test_strategy.md)

## Methods

### Public methods

- [`TestStrategy$execute()`](#method-TestStrategy-execute)

- [`TestStrategy$clone()`](#method-TestStrategy-clone)

------------------------------------------------------------------------

### Method `execute()`

Execute the test strategy

#### Usage

    TestStrategy$execute(path, plan, reporter)

#### Arguments

- `path`:

  The path to the test directory

- `plan`:

  The current mutation plan. See
  [`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md).

- `reporter`:

  The reporter to use for test results

#### Returns

The test result

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TestStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
