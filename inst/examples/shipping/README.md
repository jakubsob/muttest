# Shipping Cost — Missing Boundary Value

## What this demonstrates

A test suite that checks only "clearly above" and "clearly below" cases will let a boundary-shift mutant survive. The surviving mutant names the exact input your tests have never exercised. Adding a test at that precise boundary value kills it.

## The function

`shipping_cost` returns a flat rate based on whether the package is over or under 5 kg. The strict `>` operator means a 5 kg package pays the lower rate; `>=` would charge it the higher rate.

## The weak test

The test confirms that a heavy package costs more than a light one. It passes inputs of 10 and 2 kg — both clearly on opposite sides of the boundary — so it never exercises the value 5 itself.

## The surviving mutant

`> → >=` survives. Changing `weight_kg > 5` to `weight_kg >= 5` only affects input `weight_kg = 5` exactly. For inputs 10 and 2, the function returns identical results under both operators, so the test cannot tell the operators apart.

In production, `>= 5` instead of `> 5` would route 5 kg shipments to the expensive tier and no test would catch the regression.

## The fix

Add a test that passes the exact boundary value `5`. With `> 5`, the condition is `FALSE` and the function returns `5.00`. With `>= 5`, it is `TRUE` and returns `15.00`. This difference kills the mutant.

## Key rule

> When a comparison mutant survives, find the boundary value implied by the operator and add a test that passes exactly that value.
