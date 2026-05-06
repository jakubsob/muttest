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
#' plan <- plan(
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
#' plan <- plan(
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
#' plan <- plan(
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
