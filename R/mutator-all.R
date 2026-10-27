#' All mutators
#'
#' Returns a flat list of every mutator muttest ships, built with default
#' arguments: all the presets plus the standalone mutators not covered by one.
#' Handy for a first exploratory run; expect a large number of mutants, so
#' prefer scoping a plan to specific presets for routine use.
#'
#' Mutators that overlap are not duplicated: [remove_negation()] is included
#' instead of [remove_condition_negation()], and `while` conditions are not
#' forced to a constant (see [condition_constant()]). [drop_argument()] is
#' unscoped, so it targets every call.
#'
#' @return A list of [Mutator] objects.
#' @export
#' @examples
#' length(all_mutators())
#'
#' \dontrun{
#' plan <- muttest_plan(
#'   source_files = "R/stats.R",
#'   mutators = all_mutators()
#' )
#' muttest(plan, "tests/testthat")
#' }
all_mutators <- function() {
  c(
    arithmetic_operators(),
    comparison_operators(),
    logical_operators(),
    membership_operators(),
    operand_elision(),
    boolean_literals(),
    na_literals(),
    numeric_literals(),
    numeric_boundaries(),
    numeric_type_swaps(),
    string_literals(),
    index_mutations(),
    sequence_mutations(),
    branch_mutations(),
    function_swaps(),
    list(
      negate_condition(),
      remove_negation(),
      remove_unary_minus(),
      replace_return_value(),
      replace_implicit_return(),
      replace_call_result(),
      delete_statement(),
      drop_argument(),
      reverse_arguments(),
      remove_validation(),
      flip_named_flag(),
      remove_pipe_stage()
    )
  )
}
