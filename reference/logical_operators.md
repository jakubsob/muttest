# Logical operator mutators

Returns a ready-made list of
[`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
mutators covering short-circuit (`&&`/`||`) and vectorised (`&`/`|`)
logical operator swaps.

## Usage

``` r
logical_operators()
```

## Value

A list of
[`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
mutators.

## Details

Use on any file with compound conditions (`if (a && b)`). A surviving
mutant from this preset typically means test inputs are symmetric — both
flags `TRUE` or both `FALSE`. Adding a test with one flag `TRUE` and the
other `FALSE` exposes the difference between `&&` and `||` and kills the
mutant.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full operator table and a worked example showing the
symmetric-input pattern.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
logical_operators()
#> [[1]]
#> Mutator: && → ||
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target "&&")
#>     )
#> 
#> [[2]]
#> Mutator: || → &&
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target "||")
#>     )
#> 
#> [[3]]
#> Mutator: & → |
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target "&")
#>     )
#> 
#> [[4]]
#> Mutator: | → &
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target "|")
#>     )
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/access.R",
  mutators = logical_operators()
)
muttest(plan, "tests/testthat")
} # }
```
