#' Mutate a binary operator
#'
#' Produces one mutant per occurrence of `from` in the source file, replacing
#' it with `to`. A surviving mutant means your tests cannot distinguish the
#' original operator from the replacement — pointing at the missing assertion
#' or input value.
#'
#' Use this when you need a specific swap not covered by the preset collections
#' ([arithmetic_operators()], [comparison_operators()], [logical_operators()]).
#'
#' @param from The operator to replace (e.g. `"+"`, `"=="`, `">"`).
#' @param to The replacement operator.
#' @return A [Mutator] object.
#' @seealso
#'   [comparison_operators()], [arithmetic_operators()], [logical_operators()]
#'   for ready-made preset lists.
#'
#'   `vignette("mutators", package = "muttest")` for the full operator
#'   reference with examples of what each preset catches.
#'
#'   `vignette("interpreting-results", package = "muttest")` to learn how to
#'   read surviving mutants and strengthen the tests they expose.
#' @export
#' @examples
#' operator("+", "-")
#' operator("==", "!=")
#' operator(">", ">=")  # probe the strict vs. non-strict boundary
operator <- function(from, to) {
  checkmate::assert_string(from, min.chars = 1)
  checkmate::assert_string(to, min.chars = 1)
  Mutator$new(
    from = from,
    to = to,
    query = sprintf("(binary_operator
      lhs: (_) @lhs
      operator: _ @target
      rhs: (_) @rhs
      (#eq? @target \"%s\")
    )", from)
  )
}
