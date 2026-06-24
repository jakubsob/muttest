# Boolean literal mutators

Returns a ready-made list of
[`boolean_literal()`](https://jakubsob.github.io/muttest/reference/boolean_literal.md)
mutators covering all canonical flips: `TRUE`/`FALSE` and `T`/`F`.

## Usage

``` r
boolean_literals()
```

## Value

A list of
[`boolean_literal()`](https://jakubsob.github.io/muttest/reference/boolean_literal.md)
mutators.

## Details

Use on any file that passes or returns boolean flags. A surviving mutant
from this preset typically means a test checks a *side effect* of the
flag (e.g. the branch taken) rather than the flag value itself — adding
`expect_true()`/`expect_false()` on the return value kills it.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full mutator table.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
boolean_literals()
#> [[1]]
#> Mutator: TRUE → FALSE
#> Query: (true) @target
#> 
#> [[2]]
#> Mutator: FALSE → TRUE
#> Query: (false) @target
#> 
#> [[3]]
#> Mutator: T → F
#> Query: (identifier) @target (#eq? @target "T")
#> 
#> [[4]]
#> Mutator: F → T
#> Query: (identifier) @target (#eq? @target "F")
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/flags.R",
  mutators = boolean_literals()
)
muttest(plan, "tests/testthat")
} # }
```
