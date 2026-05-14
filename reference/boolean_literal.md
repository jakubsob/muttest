# Mutate a boolean literal

Replaces a boolean constant (`TRUE`, `FALSE`, `T`, or `F`) with another
one.

## Usage

``` r
boolean_literal(from, to)
```

## Arguments

- from:

  The literal to be replaced. One of `"TRUE"`, `"FALSE"`, `"T"`, `"F"`.

- to:

  The literal to replace with.

## Examples

``` r
boolean_literal("TRUE", "FALSE")
#> Mutator: TRUE → FALSE
#> Query: (true) @value
boolean_literal("FALSE", "TRUE")
#> Mutator: FALSE → TRUE
#> Query: (false) @value
boolean_literal("T", "F")
#> Mutator: T → F
#> Query: (identifier) @value (#eq? @value "T")
boolean_literal("F", "T")
#> Mutator: F → T
#> Query: (identifier) @value (#eq? @value "F")
```
