# Remove logical negation

Removes the `!` unary operator from an expression. For example,
`!is.na(x)` becomes `is.na(x)` and `!(a > b)` becomes `(a > b)`.

## Usage

``` r
remove_negation()
```

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Examples

``` r
remove_negation()
#> Mutator: !<expr> → <expr>
#> Query: (unary_operator) @whole
```
