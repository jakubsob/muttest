# `is_adult` — Missing Boundary Value

## What this demonstrates

A test suite that checks only values clearly on one side of a threshold will let boundary-shift comparison mutants survive. The surviving mutant names the exact input your tests have never exercised. Adding a test at that precise boundary kills it.

## The function

`is_adult` returns `TRUE` when age is 18 or older, using `>=`. Swapping to `>` would incorrectly classify an 18-year-old as a minor.

## The weak test

The tests check age 25 (clearly adult) and age 10 (clearly minor). Both inputs return the same result whether the operator is `>=` or `>`, so the tests cannot tell the operators apart.

## The surviving mutant

`>= → >` survives. Changing `age >= 18` to `age > 18` only affects one input — `age = 18` exactly. The tests never pass 18, so the function behaves identically for 25 and 10 under both operators.

## The fix

Add a test at the boundary value 18. With `>= 18`, the condition is `TRUE` and the function returns `TRUE`. With `> 18`, it is `FALSE` and returns `FALSE`. This difference kills the mutant.

## Key rule

> When a comparison mutant survives, find the boundary value implied by the operator and add a test that passes exactly that value.
