# Numeric literal mutators

Returns a ready-made list of
[`numeric_increment()`](https://jakubsob.github.io/muttest/reference/numeric_increment.md)
and
[`numeric_decrement()`](https://jakubsob.github.io/muttest/reference/numeric_decrement.md)
mutators that shift every numeric literal by 1.

## Usage

``` r
numeric_literals()
```

## Value

A list of mutators.

## Details

Use on any file with numeric constants used as thresholds, counts, or
offsets. A surviving mutant means tests never verify the exact value of
the constant — asserting the precise numeric result rather than a
property (e.g. sign) kills it.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full mutator table.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
numeric_literals()
#> [[1]]
#> Mutator: <n> + 1 → <n> + 1
#> Query: [(float) (integer)] @value
#> 
#> [[2]]
#> Mutator: <n> - 1 → <n> - 1
#> Query: [(float) (integer)] @value
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/thresholds.R",
  mutators = numeric_literals()
)
muttest(plan, "tests/testthat")
} # }
```
