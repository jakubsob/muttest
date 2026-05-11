condition_query <- function(statements) {
  parts <- vapply(statements, function(s) {
    node <- switch(s,
      "if"    = "if_statement",
      "while" = "while_statement",
      stop("Unknown statement type: ", s, ". Must be 'if' or 'while'.")
    )
    sprintf("(%s condition: (_) @cond)", node)
  }, character(1))
  if (length(parts) == 1) parts else paste0("[", paste(parts, collapse = " "), "]")
}

#' Negate the condition of if/while statements
#'
#' Wraps the condition expression of each matching statement in `!(...)`.
#' For example, `if (x > 0)` becomes `if (!(x > 0))`.
#'
#' @param statements Character vector of statement types to target.
#'   Must be a subset of `c("if", "while")`. Defaults to both.
#' @return A [Mutator] object.
#' @export
#' @examples
#' negate_condition()
#' negate_condition(statements = "if")
negate_condition <- function(statements = c("if", "while")) {
  checkmate::assert_subset(statements, c("if", "while"), empty.ok = FALSE)
  Mutator$new(
    from = "<condition>",
    to = "!(<condition>)",
    query = condition_query(statements),
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
#' @param statements Character vector of statement types to target.
#'   Must be a subset of `c("if", "while")`. Defaults to both.
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_condition_negation()
#' remove_condition_negation(statements = "while")
remove_condition_negation <- function(statements = c("if", "while")) {
  checkmate::assert_subset(statements, c("if", "while"), empty.ok = FALSE)
  Mutator$new(
    from = "!<condition>",
    to = "<condition>",
    query = condition_query(statements),
    match_fn = function(text) startsWith(text, "!"),
    replacement_fn = function(text) substring(text, 2)
  )
}
