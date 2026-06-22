#' Mutate an NA or NULL literal
#'
#' Replaces `NA`, `NULL`, or a typed NA constant (`NA_real_`, `NA_integer_`,
#' `NA_complex_`, `NA_character_`) with another value.
#'
#' In tree-sitter-r, `NA` and all typed NA variants share the same `na` node
#' type, while `NULL` has its own `null` node type. `match_fn` distinguishes
#' between them by comparing the literal text.
#'
#' @param from The literal to replace. One of `"NA"`, `"NULL"`, `"NA_real_"`,
#'   `"NA_integer_"`, `"NA_complex_"`, `"NA_character_"`.
#' @param to The replacement literal.
#' @return A [Mutator] object.
#' @export
#' @examples
#' na_literal("NULL", "NA")
#' na_literal("NA", "NULL")
#' na_literal("NA", "NA_real_")
#' na_literal("NA_real_", "NA")
na_literal <- function(from, to) {
  allowed <- c("NA", "NULL", "NA_real_", "NA_integer_", "NA_complex_", "NA_character_")
  checkmate::assert_choice(from, allowed)
  checkmate::assert_string(to, min.chars = 1)
  query <- if (from == "NULL") "(null) @target" else "(na) @target"
  Mutator$new(
    from = from,
    to = to,
    query = query,
    match_fn = function(text) text == from
  )
}
