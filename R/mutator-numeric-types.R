#' Convert integer literals to doubles
#'
#' Replaces an integer literal such as `5L` with the double `5`. Catches code
#' whose behaviour depends on the numeric type (`identical()`, `is.integer()`,
#' integer overflow, `%/%` results).
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' integer_to_double()
integer_to_double <- function() {
  Mutator$new(
    from = "<n>L",
    to = "<n>",
    query = "(integer) @target",
    match_fn = function(text) grepl("^[0-9]+L$", text),
    replacement_fn = function(text) sub("L$", "", text)
  )
}

#' Convert whole-number double literals to integers
#'
#' Replaces a whole-number double literal such as `5` with the integer `5L`.
#' Literals with a decimal point or exponent are left alone. Many of these
#' mutants are equivalent (e.g. `x[1]` vs `x[1L]`); survivors are only
#' interesting where the numeric type is observable.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' double_to_integer()
double_to_integer <- function() {
  Mutator$new(
    from = "<n>",
    to = "<n>L",
    query = "(float) @target",
    match_fn = function(text) grepl("^[0-9]+$", text),
    replacement_fn = function(text) paste0(text, "L")
  )
}

#' Replace numeric literals with boundary values
#'
#' Complements [numeric_increment()] and [numeric_decrement()] by replacing
#' every non-zero numeric literal `n` with a value that probes a different
#' failure mode: `0` (division by zero, empty results), `-n` (sign handling) or
#' `2 * n` (scaling errors). Zero literals are never mutated.
#'
#' @param kind One of `"zero"`, `"negate"` or `"double"`.
#' @return A [Mutator] object.
#' @export
#' @examples
#' numeric_replace("zero")
#' numeric_replace("negate")
#' numeric_replace("double")
numeric_replace <- function(kind = c("zero", "negate", "double")) {
  kind <- match.arg(kind)
  value <- function(text) suppressWarnings(as.numeric(sub("L$", "", text)))
  suffix <- function(text) if (endsWith(text, "L")) "L" else ""
  Mutator$new(
    from = "<n>",
    to = switch(kind, zero = "0", negate = "-<n>", double = "<n> * 2"),
    query = "[(float) (integer)] @target",
    match_fn = function(text) {
      v <- value(text)
      !is.na(v) && v != 0
    },
    replacement_fn = function(text) {
      switch(kind,
        zero = paste0("0", suffix(text)),
        negate = paste0("-", text),
        double = paste0(format(value(text) * 2, scientific = FALSE), suffix(text))
      )
    }
  )
}
