# Condition mutation mutators

Returns a ready-made list of
[`negate_condition()`](https://jakubsob.github.io/muttest/reference/negate_condition.md)
and
[`remove_condition_negation()`](https://jakubsob.github.io/muttest/reference/remove_condition_negation.md)
mutators covering both directions of condition logic: wrapping a plain
condition in `!(...)` and stripping `!` from an already-negated
condition.

## Usage

``` r
condition_mutations()
```

## Value

A list of mutators.

## Details

Use on any file with non-trivial `if`/`while` conditions. Surviving
mutants mean the branch outcome was never tested with inputs that cross
the boundary — adding a test where the condition flips from `TRUE` to
`FALSE` kills them.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full mutator table.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
condition_mutations()
#> [[1]]
#> Mutator: <condition> → !(<condition>)
#> Query: [(if_statement condition: (_) @cond) (while_statement condition: (_) @cond)]
#> 
#> [[2]]
#> Mutator: !<condition> → <condition>
#> Query: [(if_statement condition: (_) @cond) (while_statement condition: (_) @cond)]
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/validation.R",
  mutators = condition_mutations()
)
muttest(plan, "tests/testthat")
} # }
```
