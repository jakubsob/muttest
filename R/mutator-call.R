#' Mutate a function call name
#'
#' Replaces a function name in a call expression with another name.
#' Useful for swapping semantically related functions such as `any`/`all`,
#' `min`/`max`, or `sum`/`prod`.
#'
#' @param from The function name to replace.
#' @param to The function name to replace with.
#'
#' @examples
#' call_name("any", "all")
#' call_name("min", "max")
#' call_name("sum", "prod")
#'
#' @export
call_name <- function(from, to) {
  checkmate::assert_string(from, min.chars = 1)
  checkmate::assert_string(to, min.chars = 1)
  Mutator$new(
    from = from,
    to = to,
    query = sprintf(
      "(call function: (identifier) @target (#eq? @target \"%s\"))",
      from
    )
  )
}
