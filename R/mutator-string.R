#' Mutate non-empty string literals to the empty string
#'
#' Replaces any non-empty string literal in the source code with `""`.
#' The empty string itself is not mutated (use [string_fill()] for that).
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' string_empty()
string_empty <- function() {
  Mutator$new(
    from = "<non-empty string>",
    to = "\"\"",
    query = "(string) @value",
    match_fn = function(text) nchar(text) > 2,
    replacement_fn = function(text) "\"\""
  )
}

#' Mutate the empty string literal to a placeholder string
#'
#' Replaces `""` with a fill string (default `"mutant"`) so that code paths
#' that depend on an empty string can be detected.
#'
#' @param fill The replacement string. Defaults to `"mutant"`. Override when
#'   the codebase already contains `"mutant"` as a meaningful value.
#' @return A [Mutator] object.
#' @export
#' @examples
#' string_fill()
#' string_fill(fill = "PLACEHOLDER")
string_fill <- function(fill = "mutant") {
  checkmate::assert_string(fill, min.chars = 1)
  quoted <- paste0("\"", fill, "\"")
  Mutator$new(
    from = "\"\"",
    to = quoted,
    query = "(string) @value",
    match_fn = function(text) text == "\"\"",
    replacement_fn = function(text) quoted
  )
}
