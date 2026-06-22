#' Increment numeric literals
#'
#' Replaces every numeric literal `n` with `n + by`.
#' Handles both integer (e.g. `5L`) and floating-point (e.g. `3.14`) literals.
#'
#' @param by The amount to add. Defaults to `1`.
#' @return A [Mutator] object.
#' @export
#' @examples
#' numeric_increment()
#' numeric_increment(by = 2)
numeric_increment <- function(by = 1) {
  checkmate::assert_number(by)
  numeric_literal_mutator(
    label = paste0("<n> + ", by),
    transform = function(text) {
      as.character(suppressWarnings(as.numeric(text)) + by)
    }
  )
}

#' Decrement numeric literals
#'
#' Replaces every numeric literal `n` with `n - by`.
#' Handles both integer (e.g. `5L`) and floating-point (e.g. `3.14`) literals.
#'
#' @param by The amount to subtract. Defaults to `1`.
#' @return A [Mutator] object.
#' @export
#' @examples
#' numeric_decrement()
#' numeric_decrement(by = 2)
numeric_decrement <- function(by = 1) {
  checkmate::assert_number(by)
  numeric_literal_mutator(
    label = paste0("<n> - ", by),
    transform = function(text) {
      as.character(suppressWarnings(as.numeric(text)) - by)
    }
  )
}

numeric_literal_mutator <- function(label, transform) {
  Mutator$new(
    from = label,
    to = label,
    # Matches both `float` (e.g. 1.5) and `integer` (e.g. 5L) nodes.
    # The predicate (#match?) is not used here; match_fn handles filtering.
    query = "[(float) (integer)] @target",
    match_fn = function(text) {
      !is.na(suppressWarnings(as.numeric(text)))
    },
    replacement_fn = transform
  )
}
