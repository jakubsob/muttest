# Package copy strategy

It copies all files and directories from the original directory to a
temporary directory.

## See also

Other CopyStrategy:
[`CopyStrategy`](https://jakubsob.github.io/muttest/reference/CopyStrategy.md),
[`default_copy_strategy()`](https://jakubsob.github.io/muttest/reference/default_copy_strategy.md)

## Super class

[`muttest::CopyStrategy`](https://jakubsob.github.io/muttest/reference/CopyStrategy.md)
-\> `PackageCopyStrategy`

## Methods

### Public methods

- [`PackageCopyStrategy$execute()`](#method-PackageCopyStrategy-execute)

- [`PackageCopyStrategy$clone()`](#method-PackageCopyStrategy-clone)

------------------------------------------------------------------------

### Method `execute()`

Copy project files, excluding hidden and temp directories

#### Usage

    PackageCopyStrategy$execute(original_dir, plan)

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

    PackageCopyStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
