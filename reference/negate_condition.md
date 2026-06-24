# Negate the condition of if/while statements

Wraps the condition expression of each matching statement in `!(...)`.
For example, `if (x > 0)` becomes `if (!(x > 0))`.

## Usage

``` r
negate_condition(statements = c("if", "while"))
```

## Arguments

- statements:

  Character vector of statement types to target. Must be a subset of
  `c("if", "while")`. Defaults to both.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Examples

``` r
negate_condition()
#> Mutator: <condition> → !(<condition>)
#> Query: [(if_statement condition: (_) @target) (while_statement condition: (_) @target)]
negate_condition(statements = "if")
#> Mutator: <condition> → !(<condition>)
#> Query: (if_statement condition: (_) @target)
```
