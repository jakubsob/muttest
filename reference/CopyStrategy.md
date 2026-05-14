# CopyStrategy interface

Extend this class to implement a custom copy strategy.

## See also

Other CopyStrategy:
[`PackageCopyStrategy`](https://jakubsob.github.io/muttest/reference/PackageCopyStrategy.md),
[`default_copy_strategy()`](https://jakubsob.github.io/muttest/reference/default_copy_strategy.md)

## Methods

### Public methods

- [`CopyStrategy$execute()`](#method-CopyStrategy-execute)

- [`CopyStrategy$clone()`](#method-CopyStrategy-clone)

------------------------------------------------------------------------

### Method `execute()`

Copy project files according to the strategy

#### Usage

    CopyStrategy$execute(original_dir)

#### Arguments

- `original_dir`:

  The original directory to copy from

- `plan`:

  The current test plan

#### Returns

The path to the temporary directory

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CopyStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
