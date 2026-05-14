# Index mutation mutators

Returns a ready-made list of
[`index_increment()`](https://jakubsob.github.io/muttest/reference/index_increment.md)
and
[`index_decrement()`](https://jakubsob.github.io/muttest/reference/index_decrement.md)
mutators that shift every simple subscript index by 1.

## Usage

``` r
index_mutations()
```

## Value

A list of mutators.

## Details

Use on any file that extracts elements from vectors or lists by
position. Surviving mutants reveal off-by-one errors where tests only
verify that *something* was extracted, not *which* element — asserting
the exact value of the extracted element kills them.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full mutator table.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
index_mutations()
#> [[1]]
#> Mutator: x[<i>] → x[<i> + 1L]
#> Query: [(subset arguments: (arguments (argument value: [(identifier) (float) (integer)] @index))) (subset2 arguments: (arguments (argument value: [(identifier) (float) (integer)] @index)))]
#> 
#> [[2]]
#> Mutator: x[<i>] → x[<i> - 1L]
#> Query: [(subset arguments: (arguments (argument value: [(identifier) (float) (integer)] @index))) (subset2 arguments: (arguments (argument value: [(identifier) (float) (integer)] @index)))]
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/selectors.R",
  mutators = index_mutations()
)
muttest(plan, "tests/testthat")
} # }
```
