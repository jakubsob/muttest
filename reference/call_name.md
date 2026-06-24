# Mutate a function call name

Replaces a function name in a call expression with another name. Useful
for swapping semantically related functions such as `any`/`all`,
`min`/`max`, or `sum`/`prod`.

## Usage

``` r
call_name(from, to)
```

## Arguments

- from:

  The function name to replace.

- to:

  The function name to replace with.

## Examples

``` r
call_name("any", "all")
#> Mutator: any → all
#> Query: (call function: (identifier) @target (#eq? @target "any"))
call_name("min", "max")
#> Mutator: min → max
#> Query: (call function: (identifier) @target (#eq? @target "min"))
call_name("sum", "prod")
#> Mutator: sum → prod
#> Query: (call function: (identifier) @target (#eq? @target "sum"))
```
