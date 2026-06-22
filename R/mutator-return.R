#' Replace the value in explicit return() calls
#'
#' Replaces the argument of every `return(expr)` with a fixed value (default
#' `"NULL"`). Tests that only check that a function returns *something* without
#' asserting the value will not kill these mutants.
#'
#' Only explicit `return()` calls are targeted. Implicit returns (the last
#' expression of a function body) are not affected.
#'
#' @param replacement Raw R source text to substitute as the return value.
#'   Defaults to `"NULL"`. Examples: `"NA"` inserts the missing value `NA`;
#'   `'"NULL"'` (inner quotes) inserts the string `"NULL"`; `'"NA"'` inserts
#'   the string `"NA"` rather than the missing value.
#' @return A [Mutator] object.
#' @export
#' @examples
#' replace_return_value()
#' replace_return_value("NA")
replace_return_value <- function(replacement = "NULL") {
  checkmate::assert_string(replacement, min.chars = 1)
  Mutator$new(
    from = "return(<value>)",
    to = paste0("return(", replacement, ")"),
    query = "(
      call function: (identifier) @keyword arguments: (arguments (argument value: (_) @target))
      (#eq? @keyword \"return\")
    )",
    match_fn = function(text) text != replacement,
    replacement_fn = function(text) replacement
  )
}
