#' Arithmetic operator mutators
#'
#' Returns a ready-made list of [operator()] mutators covering common
#' arithmetic swaps: `+`/`-`, `*`/`/`, `^`/`*`, `%%`/`*`, `%/%`/`/`.
#'
#' Use on any file that performs calculations. A surviving mutant from this
#' preset typically means an assertion checks a *property* of the result (sign,
#' order) rather than a specific value — replacing `expect_gte()` with
#' `expect_equal()` and a computed expected value usually kills it.
#'
#' @return A list of [operator()] mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full operator table
#'   and a worked example showing the direction-insensitive assertion pattern.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' arithmetic_operators()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/stats.R",
#'   mutators = arithmetic_operators()
#' )
#' muttest(plan, "tests/testthat")
#' }
arithmetic_operators <- function() {
  list(
    operator("+", "-"),
    operator("-", "+"),
    operator("*", "/"),
    operator("/", "*"),
    operator("^", "*"),
    operator("%%", "*"),
    operator("%/%", "/")
  )
}

#' Comparison operator mutators
#'
#' Returns a ready-made list of [operator()] mutators covering direction swaps
#' (`<`/`>`, `<=`/`>=`, `==`/`!=`) and boundary shifts (`<`/`<=`, `>`/`>=`).
#'
#' Use on any file with threshold logic, range checks, or filter conditions.
#' A surviving mutant from this preset means the exact boundary value implied
#' by the operator was never passed to the function — adding a test at that
#' boundary value kills it.
#'
#' @return A list of [operator()] mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full operator table
#'   and a worked example showing the missing boundary value pattern.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' comparison_operators()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/shipping.R",
#'   mutators = comparison_operators()
#' )
#' muttest(plan, "tests/testthat")
#' }
comparison_operators <- function() {
  list(
    operator("<", ">"),
    operator(">", "<"),
    operator("<=", ">="),
    operator(">=", "<="),
    operator("==", "!="),
    operator("!=", "=="),
    operator("<", "<="),
    operator(">", ">="),
    operator("<=", "<"),
    operator(">=", ">")
  )
}

#' Logical operator mutators
#'
#' Returns a ready-made list of [operator()] mutators covering short-circuit
#' (`&&`/`||`) and vectorised (`&`/`|`) logical operator swaps.
#'
#' Use on any file with compound conditions (`if (a && b)`). A surviving mutant
#' from this preset typically means test inputs are symmetric — both flags
#' `TRUE` or both `FALSE`. Adding a test with one flag `TRUE` and the other
#' `FALSE` exposes the difference between `&&` and `||` and kills the mutant.
#'
#' @return A list of [operator()] mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full operator table
#'   and a worked example showing the symmetric-input pattern.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' logical_operators()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/access.R",
#'   mutators = logical_operators()
#' )
#' muttest(plan, "tests/testthat")
#' }
logical_operators <- function() {
  list(
    operator("&&", "||"),
    operator("||", "&&"),
    operator("&", "|"),
    operator("|", "&")
  )
}

#' Boolean literal mutators
#'
#' Returns a ready-made list of [boolean_literal()] mutators covering all
#' canonical flips: `TRUE`/`FALSE` and `T`/`F`.
#'
#' Use on any file that passes or returns boolean flags. A surviving mutant from
#' this preset typically means a test checks a *side effect* of the flag (e.g.
#' the branch taken) rather than the flag value itself — adding
#' `expect_true()`/`expect_false()` on the return value kills it.
#'
#' @return A list of [boolean_literal()] mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full mutator table.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' boolean_literals()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/flags.R",
#'   mutators = boolean_literals()
#' )
#' muttest(plan, "tests/testthat")
#' }
boolean_literals <- function() {
  list(
    boolean_literal("TRUE", "FALSE"),
    boolean_literal("FALSE", "TRUE"),
    boolean_literal("T", "F"),
    boolean_literal("F", "T")
  )
}

#' NA and NULL literal mutators
#'
#' Returns a ready-made list of [na_literal()] mutators covering common swaps
#' between `NA`, `NULL`, and the typed NA variants (`NA_real_`, `NA_integer_`,
#' `NA_character_`).
#'
#' Use on any file that handles missing values or nullable results. A surviving
#' mutant typically means tests never distinguish `NA` from `NULL`, or never
#' check which typed NA is returned — adding `expect_true(is.na(x))` and
#' `expect_equal(class(x), "numeric")` style assertions kills it.
#'
#' @return A list of [na_literal()] mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full mutator table.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' na_literals()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/missing.R",
#'   mutators = na_literals()
#' )
#' muttest(plan, "tests/testthat")
#' }
na_literals <- function() {
  list(
    na_literal("NA", "NULL"),
    na_literal("NULL", "NA"),
    na_literal("NA", "NA_real_"),
    na_literal("NA_real_", "NA"),
    na_literal("NA", "NA_integer_"),
    na_literal("NA_integer_", "NA"),
    na_literal("NA", "NA_character_"),
    na_literal("NA_character_", "NA")
  )
}

#' Numeric literal mutators
#'
#' Returns a ready-made list of [numeric_increment()] and [numeric_decrement()]
#' mutators that shift every numeric literal by 1.
#'
#' Use on any file with numeric constants used as thresholds, counts, or offsets.
#' A surviving mutant means tests never verify the exact value of the constant —
#' asserting the precise numeric result rather than a property (e.g. sign) kills it.
#'
#' @return A list of mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full mutator table.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' numeric_literals()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/thresholds.R",
#'   mutators = numeric_literals()
#' )
#' muttest(plan, "tests/testthat")
#' }
numeric_literals <- function() {
  list(
    numeric_increment(),
    numeric_decrement()
  )
}

#' Index mutation mutators
#'
#' Returns a ready-made list of [index_increment()] and [index_decrement()]
#' mutators that shift every simple subscript index by 1.
#'
#' Use on any file that extracts elements from vectors or lists by position.
#' Surviving mutants reveal off-by-one errors where tests only verify that
#' *something* was extracted, not *which* element — asserting the exact value
#' of the extracted element kills them.
#'
#' @return A list of mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full mutator table.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' index_mutations()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/selectors.R",
#'   mutators = index_mutations()
#' )
#' muttest(plan, "tests/testthat")
#' }
index_mutations <- function() {
  list(
    index_increment(),
    index_decrement()
  )
}

#' String literal mutators
#'
#' Returns a ready-made list of [string_empty()] and [string_fill()] mutators
#' covering both directions: collapsing non-empty strings to `""` and filling
#' empty strings with a placeholder.
#'
#' Use on any file where string values are passed to downstream logic or
#' returned to callers. Surviving mutants reveal tests that only check
#' *type* or *length* — asserting the exact string content kills them.
#'
#' @return A list of mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full mutator table.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' string_literals()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/labels.R",
#'   mutators = string_literals()
#' )
#' muttest(plan, "tests/testthat")
#' }
string_literals <- function() {
  list(
    string_empty(),
    string_fill()
  )
}

#' Condition mutation mutators
#'
#' Returns a ready-made list of [negate_condition()] and
#' [remove_condition_negation()] mutators covering both directions of condition
#' logic: wrapping a plain condition in `!(...)` and stripping `!` from an
#' already-negated condition.
#'
#' Use on any file with non-trivial `if`/`while` conditions. Surviving mutants
#' mean the branch outcome was never tested with inputs that cross the boundary —
#' adding a test where the condition flips from `TRUE` to `FALSE` kills them.
#'
#' @return A list of mutators.
#' @seealso
#'   `vignette("mutators", package = "muttest")` for the full mutator table.
#'
#'   `vignette("interpreting-results", package = "muttest")` for how to
#'   diagnose survivors and fix the underlying test weakness.
#' @export
#' @examples
#' condition_mutations()
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/validation.R",
#'   mutators = condition_mutations()
#' )
#' muttest(plan, "tests/testthat")
#' }
condition_mutations <- function() {
  list(
    negate_condition(),
    remove_condition_negation()
  )
}
