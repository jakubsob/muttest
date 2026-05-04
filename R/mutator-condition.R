#' Negate the condition of if/while statements
#'
#' Wraps the condition expression of each `if` or `while` statement in `!(...)`.
#' For example, `if (x > 0)` becomes `if (!(x > 0))`.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' negate_condition()
negate_condition <- function() {
  Mutator$new(
    from = "<condition>",
    to = "!(<condition>)",
    query = "[(if_statement condition: (_) @cond) (while_statement condition: (_) @cond)]",
    match_fn = function(text) TRUE,
    replacement_fn = function(text) paste0("!(", text, ")")
  )
}

#' Remove negation from the condition of if/while statements
#'
#' The inverse of [negate_condition()]. Strips the leading `!` from any
#' already-negated condition, so `if (!done)` becomes `if (done)` and
#' `while (!ready)` becomes `while (ready)`.
#'
#' Unlike [remove_negation()], this mutator is scoped exclusively to
#' conditions, leaving negations in other positions (assignments, return
#' values, etc.) untouched.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_condition_negation()
remove_condition_negation <- function() {
  Mutator$new(
    from = "!<condition>",
    to = "<condition>",
    query = "[(if_statement condition: (_) @cond) (while_statement condition: (_) @cond)]",
    match_fn = function(text) startsWith(text, "!"),
    replacement_fn = function(text) substring(text, 2)
  )
}
