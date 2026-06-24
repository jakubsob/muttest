# String literal mutators

Returns a ready-made list of
[`string_empty()`](https://jakubsob.github.io/muttest/reference/string_empty.md)
and
[`string_fill()`](https://jakubsob.github.io/muttest/reference/string_fill.md)
mutators covering both directions: collapsing non-empty strings to `""`
and filling empty strings with a placeholder.

## Usage

``` r
string_literals()
```

## Value

A list of mutators.

## Details

Use on any file where string values are passed to downstream logic or
returned to callers. Surviving mutants reveal tests that only check
*type* or *length* — asserting the exact string content kills them.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full mutator table.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
string_literals()
#> [[1]]
#> Mutator: <non-empty string> → ""
#> Query: (string) @target
#> 
#> [[2]]
#> Mutator: "" → "mutant"
#> Query: (string) @target
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/labels.R",
  mutators = string_literals()
)
muttest(plan, "tests/testthat")
} # }
```
