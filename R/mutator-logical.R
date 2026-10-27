#' Keep only one operand of a logical operator
#'
#' Replaces `a && b` (and `||`, `&`, `|`) with just the left or right operand.
#' Stronger than swapping `&&` for `||`: a surviving mutant proves the dropped
#' operand never changes any test outcome.
#'
#' @param keep Which operand to keep, `"lhs"` or `"rhs"`.
#' @return A [Mutator] object.
#' @export
#' @examples
#' keep_operand("lhs")
#' keep_operand("rhs")
keep_operand <- function(keep = c("lhs", "rhs")) {
  keep <- match.arg(keep)
  node_mutator(
    from = "a && b",
    to = if (keep == "lhs") "a" else "b",
    query = "(binary_operator) @target",
    propose = function(node) {
      if (!field_text(node, "operator") %in% c("&&", "||", "&", "|")) return(NULL)
      list(proposal(node, field_text(node, keep)))
    }
  )
}
