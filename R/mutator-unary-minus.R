#' Remove unary minus
#'
#' Strips the leading `-` from a negated expression, so `-x` becomes `x` and
#' `-(a + b)` becomes `(a + b)`. Catches tests that never check the sign of a
#' result.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_unary_minus()
remove_unary_minus <- function() {
  Mutator$new(
    from = "-<expr>",
    to = "<expr>",
    query = "(unary_operator) @target",
    match_fn = function(text) startsWith(text, "-"),
    replacement_fn = function(text) substring(text, 2)
  )
}
