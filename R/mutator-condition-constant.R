#' Replace if/while conditions with a constant
#'
#' Replaces the condition of each matching statement with `TRUE` or `FALSE`,
#' so that a branch is always (or never) taken. Surviving mutants mean no test
#' distinguishes the real condition from a constant.
#'
#' Targets `if` only by default. `while (TRUE)` mutants normally loop forever
#' and `while (FALSE)` skips the loop entirely, so opt in to `"while"` only if
#' your tests have a timeout.
#'
#' @param value `TRUE` or `FALSE`, the constant to substitute.
#' @param statements Character vector of statement types to target; a subset
#'   of `c("if", "while")`.
#' @return A [Mutator] object.
#' @export
#' @examples
#' condition_constant(TRUE)
#' condition_constant(FALSE)
condition_constant <- function(value = TRUE, statements = "if") {
  checkmate::assert_flag(value)
  checkmate::assert_subset(statements, c("if", "while"), empty.ok = FALSE)
  replacement <- as.character(value)
  Mutator$new(
    from = "<condition>",
    to = replacement,
    query = condition_query(statements),
    match_fn = function(text) text != replacement,
    replacement_fn = function(text) replacement
  )
}
