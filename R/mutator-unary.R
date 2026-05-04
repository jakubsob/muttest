#' Remove logical negation
#'
#' Removes the `!` unary operator from an expression.
#' For example, `!is.na(x)` becomes `is.na(x)` and `!(a > b)` becomes `(a > b)`.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_negation()
remove_negation <- function() {
  Mutator$new(
    from = "!<expr>",
    to = "<expr>",
    query = "(unary_operator) @whole",
    match_fn = function(text) startsWith(text, "!"),
    replacement_fn = function(text) substring(text, 2)
  )
}
