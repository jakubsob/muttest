# Mutate an NA or NULL literal

Replaces `NA`, `NULL`, or a typed NA constant (`NA_real_`,
`NA_integer_`, `NA_complex_`, `NA_character_`) with another value.

## Usage

``` r
na_literal(from, to)
```

## Arguments

- from:

  The literal to replace. One of `"NA"`, `"NULL"`, `"NA_real_"`,
  `"NA_integer_"`, `"NA_complex_"`, `"NA_character_"`.

- to:

  The replacement literal.

## Value

A [Mutator](https://jakubsob.github.io/muttest/reference/Mutator.md)
object.

## Details

In tree-sitter-r, `NA` and all typed NA variants share the same `na`
node type, while `NULL` has its own `null` node type. `match_fn`
distinguishes between them by comparing the literal text.

## Examples

``` r
na_literal("NULL", "NA")
#> Mutator: NULL → NA
#> Query: (null) @target
na_literal("NA", "NULL")
#> Mutator: NA → NULL
#> Query: (na) @target
na_literal("NA", "NA_real_")
#> Mutator: NA → NA_real_
#> Query: (na) @target
na_literal("NA_real_", "NA")
#> Mutator: NA_real_ → NA
#> Query: (na) @target
```
