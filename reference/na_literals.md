# NA and NULL literal mutators

Returns a ready-made list of
[`na_literal()`](https://jakubsob.github.io/muttest/reference/na_literal.md)
mutators covering common swaps between `NA`, `NULL`, and the typed NA
variants (`NA_real_`, `NA_integer_`, `NA_character_`).

## Usage

``` r
na_literals()
```

## Value

A list of
[`na_literal()`](https://jakubsob.github.io/muttest/reference/na_literal.md)
mutators.

## Details

Use on any file that handles missing values or nullable results. A
surviving mutant typically means tests never distinguish `NA` from
`NULL`, or never check which typed NA is returned — adding
`expect_true(is.na(x))` and `expect_equal(class(x), "numeric")` style
assertions kills it.

## See also

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full mutator table.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
for how to diagnose survivors and fix the underlying test weakness.

## Examples

``` r
na_literals()
#> [[1]]
#> Mutator: NA → NULL
#> Query: (na) @target
#> 
#> [[2]]
#> Mutator: NULL → NA
#> Query: (null) @target
#> 
#> [[3]]
#> Mutator: NA → NA_real_
#> Query: (na) @target
#> 
#> [[4]]
#> Mutator: NA_real_ → NA
#> Query: (na) @target
#> 
#> [[5]]
#> Mutator: NA → NA_integer_
#> Query: (na) @target
#> 
#> [[6]]
#> Mutator: NA_integer_ → NA
#> Query: (na) @target
#> 
#> [[7]]
#> Mutator: NA → NA_character_
#> Query: (na) @target
#> 
#> [[8]]
#> Mutator: NA_character_ → NA
#> Query: (na) @target
#> 

if (FALSE) { # \dontrun{
plan <- muttest_plan(
  source_files = "R/missing.R",
  mutators = na_literals()
)
muttest(plan, "tests/testthat")
} # }
```
