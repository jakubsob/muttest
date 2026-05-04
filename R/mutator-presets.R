#' Arithmetic operator mutators
#'
#' Returns a list of mutators covering common arithmetic operator replacements.
#'
#' @return A list of [operator()] mutators.
#' @export
#' @examples
#' arithmetic_operators()
arithmetic_operators <- function() {
  list(
    operator("+", "-"),
    operator("-", "+"),
    operator("*", "/"),
    operator("/", "*"),
    operator("^", "*"),
    operator("%%", "*"),
    operator("%/%", "/")
  )
}

#' Comparison operator mutators
#'
#' Returns a list of mutators covering comparison operator replacements,
#' including boundary shifts (e.g. `<` to `<=`).
#'
#' @return A list of [operator()] mutators.
#' @export
#' @examples
#' comparison_operators()
comparison_operators <- function() {
  list(
    operator("<", ">"),
    operator(">", "<"),
    operator("<=", ">="),
    operator(">=", "<="),
    operator("==", "!="),
    operator("!=", "=="),
    operator("<", "<="),
    operator(">", ">="),
    operator("<=", "<"),
    operator(">=", ">")
  )
}

#' Logical operator mutators
#'
#' Returns a list of mutators covering logical operator replacements,
#' for both short-circuit (`&&`/`||`) and vectorised (`&`/`|`) forms.
#'
#' @return A list of [operator()] mutators.
#' @export
#' @examples
#' logical_operators()
logical_operators <- function() {
  list(
    operator("&&", "||"),
    operator("||", "&&"),
    operator("&", "|"),
    operator("|", "&")
  )
}
