# Replace the value in explicit return() calls

Replaces the argument of every `return(expr)` with a fixed value
(default `"NULL"`). Tests that only check that a function returns
*something* without asserting the value will not kill these mutants.

## Usage

``` r
replace_return_value(replacement = "NULL")
```

## Arguments

- replacement:

  Raw R source text to substitute as the return value. Defaults to
  `"NULL"`. Examples: `"NA"` inserts the missing value `NA`; `'"NULL"'`
  (inner quotes) inserts the string `"NULL"`; `'"NA"'` inserts the
  string `"NA"` rather than the missing value.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Details

Only explicit [`return()`](https://rdrr.io/r/base/function.html) calls
are targeted. Implicit returns (the last expression of a function body)
are not affected.

## Examples

``` r
replace_return_value()
#> Mutator: return(<value>) → return(NULL)
#> Query: (
#>       call function: (identifier) @keyword arguments: (arguments (argument value: (_) @target))
#>       (#eq? @keyword "return")
#>     )
replace_return_value("NA")
#> Mutator: return(<value>) → return(NA)
#> Query: (
#>       call function: (identifier) @keyword arguments: (arguments (argument value: (_) @target))
#>       (#eq? @keyword "return")
#>     )
```
