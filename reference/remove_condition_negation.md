# Remove negation from the condition of if/while statements

The inverse of
[`negate_condition()`](https://jakubsob.github.io/muttest/reference/negate_condition.md).
Strips the leading `!` from any already-negated condition, so
`if (!done)` becomes `if (done)` and `while (!ready)` becomes
`while (ready)`.

## Usage

``` r
remove_condition_negation(statements = c("if", "while"))
```

## Arguments

- statements:

  Character vector of statement types to target. Must be a subset of
  `c("if", "while")`. Defaults to both.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Details

Unlike
[`remove_negation()`](https://jakubsob.github.io/muttest/reference/remove_negation.md),
this mutator is scoped exclusively to conditions, leaving negations in
other positions (assignments, return values, etc.) untouched.

## Examples

``` r
remove_condition_negation()
#> Mutator: !<condition> → <condition>
#> Query: [(if_statement condition: (_) @target) (while_statement condition: (_) @target)]
remove_condition_negation(statements = "while")
#> Mutator: !<condition> → <condition>
#> Query: (while_statement condition: (_) @target)
```
