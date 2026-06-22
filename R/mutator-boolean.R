#' Mutate a boolean literal
#'
#' Replaces a boolean constant (`TRUE`, `FALSE`, `T`, or `F`) with another one.
#'
#' @param from The literal to be replaced. One of `"TRUE"`, `"FALSE"`, `"T"`, `"F"`.
#' @param to The literal to replace with.
#'
#' @examples
#' boolean_literal("TRUE", "FALSE")
#' boolean_literal("FALSE", "TRUE")
#' boolean_literal("T", "F")
#' boolean_literal("F", "T")
#'
#' @export
boolean_literal <- function(from, to) {
  allowed <- c("TRUE", "FALSE", "T", "F")
  checkmate::assert_choice(from, allowed)
  checkmate::assert_choice(to, allowed)
  # TRUE/FALSE are dedicated AST nodes; T/F are plain identifiers in tree-sitter-r
  query <- if (from %in% c("TRUE", "FALSE")) {
    node_type <- if (from == "TRUE") "true" else "false"
    sprintf("(%s) @target", node_type)
  } else {
    sprintf("(identifier) @target (#eq? @target \"%s\")", from)
  }
  Mutator$new(from = from, to = to, query = query)
}
