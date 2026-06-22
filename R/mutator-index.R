#' Increment subscript indices
#'
#' Replaces every simple subscript index in `x[i]` or `x[[i]]` with
#' `x[i + 1L]` / `x[[i + 1L]]`. Targets identifier and numeric literal indices;
#' complex expressions (e.g. `x[a + b]`) are left untouched.
#'
#' Catches off-by-one errors where tests never verify the exact element
#' retrieved from a vector or list.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' index_increment()
index_increment <- function() {
  index_mutator("<i> + 1L", function(text) paste0(text, " + 1L"))
}

#' Decrement subscript indices
#'
#' Replaces every simple subscript index in `x[i]` or `x[[i]]` with
#' `x[i - 1L]` / `x[[i - 1L]]`. Targets identifier and numeric literal indices;
#' complex expressions (e.g. `x[a + b]`) are left untouched.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' index_decrement()
index_decrement <- function() {
  index_mutator("<i> - 1L", function(text) paste0(text, " - 1L"))
}

index_mutator <- function(label, transform) {
  Mutator$new(
    from = "x[<i>]",
    to = paste0("x[", label, "]"),
    query = paste0(
      "[(subset arguments: (arguments (argument value: [(identifier) (float) (integer)] @target)))",
      " (subset2 arguments: (arguments (argument value: [(identifier) (float) (integer)] @target)))]"
    ),
    match_fn = function(text) TRUE,
    replacement_fn = transform
  )
}
