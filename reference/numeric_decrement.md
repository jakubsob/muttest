# Decrement numeric literals

Replaces every numeric literal `n` with `n - by`. Handles both integer
(e.g. `5L`) and floating-point (e.g. `3.14`) literals.

## Usage

``` r
numeric_decrement(by = 1)
```

## Arguments

- by:

  The amount to subtract. Defaults to `1`.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Examples

``` r
numeric_decrement()
#> Mutator: <n> - 1 → <n> - 1
#> Query: [(float) (integer)] @value
numeric_decrement(by = 2)
#> Mutator: <n> - 2 → <n> - 2
#> Query: [(float) (integer)] @value
```
