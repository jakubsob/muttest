# Comparison operator mutators

Returns a ready-made list of
[`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
mutators covering direction swaps (`<`/`>`, `<=`/`>=`, `==`/`!=`) and
boundary shifts (`<`/`<=`, `>`/`>=`).

## Usage

``` r
comparison_operators()
```

## Value

A list of
[`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
mutators.

## Details

Use on any file with threshold logic, range checks, or filter
conditions. A surviving mutant from this preset means the exact boundary
value implied by the operator was never passed to the function — adding
a test at that boundary value kills it.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full operator table and a worked example showing the missing
boundary value pattern.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
comparison_operators()
#> [[1]]
#> Mutator: < → >
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "<")
#>     )
#> 
#> [[2]]
#> Mutator: > → <
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator ">")
#>     )
#> 
#> [[3]]
#> Mutator: <= → >=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "<=")
#>     )
#> 
#> [[4]]
#> Mutator: >= → <=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator ">=")
#>     )
#> 
#> [[5]]
#> Mutator: == → !=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "==")
#>     )
#> 
#> [[6]]
#> Mutator: != → ==
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "!=")
#>     )
#> 
#> [[7]]
#> Mutator: < → <=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "<")
#>     )
#> 
#> [[8]]
#> Mutator: > → >=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator ">")
#>     )
#> 
#> [[9]]
#> Mutator: <= → <
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator "<=")
#>     )
#> 
#> [[10]]
#> Mutator: >= → >
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @operator
#>       rhs: (_) @rhs
#>       (#eq? @operator ">=")
#>     )
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/shipping.R",
  mutators = comparison_operators()
)
muttest(plan, "tests/testthat")
} # }
```
