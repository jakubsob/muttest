# Label — Property Check Instead of Exact Value

## What this demonstrates

Assertions that check only whether a string is non-empty (or has characters) cannot detect string literal mutations. Replacing a string with `""` or `"mutant"` changes the content but may still satisfy a length check. Asserting the exact expected string kills the mutant.

## The function

`greet` returns a personalised greeting or a fallback for empty names. `default_label` returns the label if non-empty, or `"unknown"` for an empty string. Both functions contain string literals that `string_empty()` and `string_fill()` will replace.

## The weak test

The tests check that the return value is a character and that its length is greater than zero. A mutated string (`""` or `"mutant"`) may still satisfy `is.character()`, and `"mutant"` satisfies `nchar() > 0`. Neither check catches the mutation.

## The surviving mutant

`string_empty()` replaces `"Hello, stranger!"` with `""`. `greet("")` now returns `""`. `is.character("")` = `TRUE` and the test passes — but the function is silently broken.

## The fix

Assert the exact string that the function should return. `expect_equal(greet("Alice"), "Hello, Alice!")` fails immediately when the literal is emptied or filled with a placeholder.

## Key rule

> When a string literal mutant survives, replace length or type checks with `expect_equal` against the exact expected string.
