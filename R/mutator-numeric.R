#' Increment numeric literals by one
#'
#' Replaces every numeric literal `n` with `n + 1`.
#' Handles both integer (e.g. `5L`) and floating-point (e.g. `3.14`) literals.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' numeric_increment()
numeric_increment <- function() {
  numeric_literal_mutator(
    label = "<n> + 1",
    transform = function(text) {
      as.character(suppressWarnings(as.numeric(text)) + 1)
    }
  )
}

#' Decrement numeric literals by one
#'
#' Replaces every numeric literal `n` with `n - 1`.
#' Handles both integer (e.g. `5L`) and floating-point (e.g. `3.14`) literals.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' numeric_decrement()
numeric_decrement <- function() {
  numeric_literal_mutator(
    label = "<n> - 1",
    transform = function(text) {
      as.character(suppressWarnings(as.numeric(text)) - 1)
    }
  )
}

numeric_literal_mutator <- function(label, transform) {
  Mutator$new(
    from = label,
    to = label,
    # Matches both `float` (e.g. 1.5) and `integer` (e.g. 5L) nodes.
    # The predicate (#match?) is not used here; match_fn handles filtering.
    query = "[(float) (integer)] @value",
    match_fn = function(text) {
      !is.na(suppressWarnings(as.numeric(text)))
    },
    replacement_fn = transform
  )
}
