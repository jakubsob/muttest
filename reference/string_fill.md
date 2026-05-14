# Mutate the empty string literal to a placeholder string

Replaces `""` with a fill string (default `"mutant"`) so that code paths
that depend on an empty string can be detected.

## Usage

``` r
string_fill(fill = "mutant")
```

## Arguments

- fill:

  The replacement string. Defaults to `"mutant"`. Override when the
  codebase already contains `"mutant"` as a meaningful value.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Examples

``` r
string_fill()
#> Mutator: "" → "mutant"
#> Query: (string) @value
string_fill(fill = "PLACEHOLDER")
#> Mutator: "" → "PLACEHOLDER"
#> Query: (string) @value
```
