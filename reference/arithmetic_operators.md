# Arithmetic operator mutators

Returns a ready-made list of
[`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
mutators covering common arithmetic swaps: `+`/`-`, `*`/`/`, `^`/`*`,
`%%`/`*`, `%/%`/`/`.

## Usage

``` r
arithmetic_operators()
```

## Value

A list of
[`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
mutators.

## Details

Use on any file that performs calculations. A surviving mutant from this
preset typically means an assertion checks a *property* of the result
(sign, order) rather than a specific value — replacing `expect_gte()`
with `expect_equal()` and a computed expected value usually kills it.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full operator table and a worked example showing the
direction-insensitive assertion pattern.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
arithmetic_operators()
#> [[1]]
#> Mutator: + → -
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "+")
#>     )
#> 
#> [[2]]
#> Mutator: - → +
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "-")
#>     )
#> 
#> [[3]]
#> Mutator: * → /
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "*")
#>     )
#> 
#> [[4]]
#> Mutator: / → *
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "/")
#>     )
#> 
#> [[5]]
#> Mutator: ^ → *
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "^")
#>     )
#> 
#> [[6]]
#> Mutator: %% → *
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "%%")
#>     )
#> 
#> [[7]]
#> Mutator: %/% → /
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "%/%")
#>     )
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/stats.R",
  mutators = arithmetic_operators()
)
muttest(plan, "tests/testthat")
} # }
```
