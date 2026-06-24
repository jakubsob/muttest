# Mutate a binary operator

Produces one mutant per occurrence of `from` in the source file,
replacing it with `to`. A surviving mutant means your tests cannot
distinguish the original operator from the replacement — pointing at the
missing assertion or input value.

## Usage

``` r
operator(from, to)
```

## Arguments

- from:

  The operator to replace (e.g. `"+"`, `"=="`, `">"`).

- to:

  The replacement operator.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Details

Use this when you need a specific swap not covered by the preset
collections
([`arithmetic_operators()`](https://jakubsob.github.io/muttest/reference/arithmetic_operators.md),
[`comparison_operators()`](https://jakubsob.github.io/muttest/reference/comparison_operators.md),
[`logical_operators()`](https://jakubsob.github.io/muttest/reference/logical_operators.md)).

## See also

[`comparison_operators()`](https://jakubsob.github.io/muttest/reference/comparison_operators.md),
[`arithmetic_operators()`](https://jakubsob.github.io/muttest/reference/arithmetic_operators.md),
[`logical_operators()`](https://jakubsob.github.io/muttest/reference/logical_operators.md)
for ready-made preset lists.

[`vignette("mutators", package = "muttest")`](https://jakubsob.github.io/muttest/articles/mutators.md)
for the full operator reference with examples of what each preset
catches.

[`vignette("interpreting-results", package = "muttest")`](https://jakubsob.github.io/muttest/articles/interpreting-results.md)
to learn how to read surviving mutants and strengthen the tests they
expose.

## Examples

``` r
operator("+", "-")
#> Mutator: + → -
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target "+")
#>     )
operator("==", "!=")
#> Mutator: == → !=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target "==")
#>     )
operator(">", ">=")  # probe the strict vs. non-strict boundary
#> Mutator: > → >=
#> Query: (binary_operator
#>       lhs: (_) @lhs
#>       operator: _ @target
#>       rhs: (_) @rhs
#>       (#eq? @target ">")
#>     )
```
