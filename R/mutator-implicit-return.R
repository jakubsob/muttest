#' Replace the implicit return value of functions
#'
#' Replaces the last expression of every function body (the implicit return
#' value) with a fixed value (default `"NULL"`). Complements
#' [replace_return_value()], which only handles explicit `return()` calls; most
#' idiomatic R relies on implicit returns. Functions whose last expression is
#' already a `return()` call are skipped.
#'
#' @param replacement Raw R source text to substitute as the value.
#' @return A [Mutator] object.
#' @export
#' @examples
#' replace_implicit_return()
#' replace_implicit_return("NA")
replace_implicit_return <- function(replacement = "NULL") {
  checkmate::assert_string(replacement, min.chars = 1)
  node_mutator(
    from = "<last expression>",
    to = replacement,
    query = "(function_definition) @target",
    propose = function(node) {
      last <- treesitter::node_child_by_field_name(node, "body")
      if (treesitter::node_type(last) == "braced_expression") {
        stmts <- Filter(
          function(n) treesitter::node_type(n) != "comment",
          treesitter::node_named_children(last)
        )
        if (length(stmts) == 0) return(NULL)
        last <- stmts[[length(stmts)]]
      }
      if (treesitter::node_text(last) == replacement) return(NULL)
      if (treesitter::node_type(last) == "call" && identical(call_function_name(last), "return")) {
        return(NULL)
      }
      list(proposal(last, replacement))
    }
  )
}

#' Replace the result of function calls
#'
#' Replaces every function call with a fixed value (default `"NULL"`), e.g.
#' `y <- compute(x)` becomes `y <- NULL`. Surviving mutants mean a call's
#' result is never checked. Calls that are assignment targets
#' (`names(x) <- v`) and `return()` calls are left alone.
#'
#' @param replacement Raw R source text to substitute for the call.
#' @return A [Mutator] object.
#' @export
#' @examples
#' replace_call_result()
#' replace_call_result("0")
replace_call_result <- function(replacement = "NULL") {
  checkmate::assert_string(replacement, min.chars = 1)
  node_mutator(
    from = "f(...)",
    to = replacement,
    query = "(call) @target",
    propose = function(node) {
      if (treesitter::node_text(node) == replacement) return(NULL)
      if (identical(call_function_name(node), "return")) return(NULL)
      if (is_assignment_target(node)) return(NULL)
      list(proposal(node, replacement))
    }
  )
}
