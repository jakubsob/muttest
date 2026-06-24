# Delete statements one at a time

Produces one mutant per deletable statement, removing each `x <- expr`
assignment or standalone `f(...)` call from the source. Surviving
mutants reveal untested side effects or dead assignments.

## Usage

``` r
delete_statement()
```

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Details

Function definitions (`x <- function(...) { ... }`) are left untouched
to avoid producing structurally broken mutants.

## Examples

``` r
delete_statement()
#> Mutator: <statement> → 
#> Query: [(program (binary_operator operator: _ @op     (#match? @op "^(<-|<<-|=)$")) @target)  (braced_expression (binary_operator operator: _ @op     (#match? @op "^(<-|<<-|=)$")) @target)  (program (call) @target)  (braced_expression (call) @target)]
```
