#' Function name swap mutators
#'
#' Returns a ready-made list of [call_name()] mutators for semantically related
#' functions: `any`/`all`, `min`/`max`, `sum`/`prod`, `head`/`tail`,
#' `which.min`/`which.max`, `cumsum`/`cumprod`, `floor`/`ceiling`,
#' `nrow`/`ncol` (and `NROW`/`NCOL`), `startsWith`/`endsWith`,
#' `toupper`/`tolower`, `isTRUE`/`isFALSE` and `nchar`/`length`.
#'
#' Use on any file that aggregates, slices or reshapes data. A surviving mutant
#' means the test data cannot tell the two functions apart, e.g. a square
#' matrix for `nrow`/`ncol` or a single-element vector for `min`/`max`.
#'
#' @return A list of [call_name()] mutators.
#' @export
#' @examples
#' function_swaps()
function_swaps <- function() {
  pairs <- list(
    c("any", "all"),
    c("min", "max"),
    c("sum", "prod"),
    c("head", "tail"),
    c("which.min", "which.max"),
    c("cumsum", "cumprod"),
    c("floor", "ceiling"),
    c("nrow", "ncol"),
    c("NROW", "NCOL"),
    c("startsWith", "endsWith"),
    c("toupper", "tolower"),
    c("isTRUE", "isFALSE"),
    c("nchar", "length")
  )
  unlist(
    lapply(pairs, function(p) list(call_name(p[1], p[2]), call_name(p[2], p[1]))),
    recursive = FALSE
  )
}

#' Membership operator mutators
#'
#' Returns a list with an [operator()] mutator that replaces `%in%` with `==`.
#' The two agree for scalar inputs but differ for vectors, so a surviving
#' mutant means tests only ever pass a single value.
#'
#' @return A list of [operator()] mutators.
#' @export
#' @examples
#' membership_operators()
membership_operators <- function() {
  list(operator("%in%", "=="))
}

#' Operand elision mutators
#'
#' Returns [keep_operand()] mutators that reduce each `&&`, `||`, `&` and `|`
#' expression to its left or right operand.
#'
#' @return A list of [keep_operand()] mutators.
#' @export
#' @examples
#' operand_elision()
operand_elision <- function() {
  list(keep_operand("lhs"), keep_operand("rhs"))
}

#' Branch mutators
#'
#' Returns mutators that force or remove branches: [condition_constant()]
#' (`TRUE` and `FALSE`), [remove_else_branch()], [empty_if_body()] and
#' [remove_null_guard()].
#'
#' Use on files with significant control flow. Surviving mutants point to
#' branches whose behaviour no test observes.
#'
#' @return A list of mutators.
#' @export
#' @examples
#' branch_mutations()
branch_mutations <- function() {
  list(
    condition_constant(TRUE),
    condition_constant(FALSE),
    remove_else_branch(),
    empty_if_body(),
    remove_null_guard()
  )
}

#' Numeric boundary mutators
#'
#' Returns [numeric_replace()] mutators that replace each non-zero numeric
#' literal with `0`, `-n` and `2 * n`. Use alongside [numeric_literals()].
#'
#' @return A list of mutators.
#' @export
#' @examples
#' numeric_boundaries()
numeric_boundaries <- function() {
  list(numeric_replace("zero"), numeric_replace("negate"), numeric_replace("double"))
}

#' Numeric type mutators
#'
#' Returns [integer_to_double()] and [double_to_integer()] mutators that flip
#' the numeric type of literals. Many resulting mutants are equivalent, so use
#' selectively on code where integer vs double behaviour matters.
#'
#' @return A list of mutators.
#' @export
#' @examples
#' numeric_type_swaps()
numeric_type_swaps <- function() {
  list(integer_to_double(), double_to_integer())
}

#' Sequence mutators
#'
#' Returns mutators for off-by-one errors in loops and ranges:
#' [seq_len_decrement()], [seq_along_drop_first()], [colon_start_increment()]
#' and [colon_end_decrement()].
#'
#' @return A list of mutators.
#' @export
#' @examples
#' sequence_mutations()
sequence_mutations <- function() {
  list(
    seq_len_decrement(),
    seq_along_drop_first(),
    colon_start_increment(),
    colon_end_decrement()
  )
}
