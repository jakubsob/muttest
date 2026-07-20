# Package copy strategy

It copies all files and directories from the original directory to a
temporary directory.

When `symlink = TRUE`, only the top-level directory holding the mutated
file (e.g. `R/`) is copied; every other top-level directory and root
file is symlinked to the original. For N mutants this avoids N full
copies, which matters when the project carries large `inst/`/`data/`
directories. Tests only read the symlinked files, so this is safe as
long as the test suite does not write into the package's own directories
(writes through a symlink would modify the original).

## See also

Other CopyStrategy:
[`CopyStrategy`](https://jakubsob.github.io/muttest/reference/CopyStrategy.md),
[`default_copy_strategy()`](https://jakubsob.github.io/muttest/reference/default_copy_strategy.md)

## Super class

[`muttest::CopyStrategy`](https://jakubsob.github.io/muttest/reference/CopyStrategy.md)
-\> `PackageCopyStrategy`

## Methods

### Public methods

- [`PackageCopyStrategy$new()`](#method-PackageCopyStrategy-new)

- [`PackageCopyStrategy$execute()`](#method-PackageCopyStrategy-execute)

- [`PackageCopyStrategy$clone()`](#method-PackageCopyStrategy-clone)

------------------------------------------------------------------------

### Method `new()`

Create a package copy strategy

#### Usage

    PackageCopyStrategy$new(symlink = FALSE)

#### Arguments

- `symlink`:

  If `TRUE`, symlink unchanged files instead of copying them.

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
