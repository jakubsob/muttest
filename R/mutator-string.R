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
#' Replaces `""` with `"mutant"` so that code paths that depend on an empty
#' string can be detected.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' string_fill()
string_fill <- function() {
  Mutator$new(
    from = "\"\"",
    to = "\"mutant\"",
    query = "(string) @value",
    match_fn = function(text) text == "\"\"",
    replacement_fn = function(text) "\"mutant\""
  )
}
