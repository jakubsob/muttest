# Decrement subscript indices

Replaces every simple subscript index in `x[i]` or `x[[i]]` with
`x[i - 1L]` / `x[[i - 1L]]`. Targets identifier and numeric literal
indices; complex expressions (e.g. `x[a + b]`) are left untouched.

## Usage

``` r
index_decrement()
```

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Examples

``` r
index_decrement()
#> Mutator: x[<i>] → x[<i> - 1L]
#> Query: [(subset arguments: (arguments (argument value: [(identifier) (float) (integer)] @index))) (subset2 arguments: (arguments (argument value: [(identifier) (float) (integer)] @index)))]
```
