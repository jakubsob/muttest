# Mutate non-empty string literals to the empty string

Replaces any non-empty string literal in the source code with `""`. The
empty string itself is not mutated (use
[`string_fill()`](https://jakubsob.github.io/muttest/reference/string_fill.md)
for that).

## Usage

``` r
string_empty()
```

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Examples

``` r
string_empty()
#> Mutator: <non-empty string> → ""
#> Query: (string) @target
```
