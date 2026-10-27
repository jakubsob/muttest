#' Drop one argument from a call
#'
#' Produces one mutant per argument of each call with two or more arguments,
#' removing that argument: `f(a, b)` becomes `f(b)` and `f(a)`. Surviving
#' mutants reveal parameters that have no observable effect. `return()` is never
#' touched. Dropping arguments often yields code that errors, which counts as
#' killed, so scope it with `functions` to keep runs fast.
#'
#' @param functions Character vector of function names to target (without
#'   `pkg::`). `NULL`, the default, targets every call.
#' @return A [Mutator] object.
#' @export
#' @examples
#' drop_argument()
#' drop_argument("c")
drop_argument <- function(functions = NULL) {
  checkmate::assert_character(functions, min.chars = 1, any.missing = FALSE, null.ok = TRUE)
  argument_mutator("f(a, b)", "f(b)", functions, function(texts) {
    lapply(seq_along(texts), function(i) texts[-i])
  })
}

#' Reverse the arguments of a call
#'
#' Reverses the argument order of each call with two or more arguments, so
#' `c(a, b)` becomes `c(b, a)`. Surviving mutants mean no test depends on the
#' order of the elements.
#'
#' @param functions Character vector of function names to target (without
#'   `pkg::`). Defaults to `"c"`; `NULL` targets every call.
#' @return A [Mutator] object.
#' @export
#' @examples
#' reverse_arguments()
#' reverse_arguments(c("c", "paste"))
reverse_arguments <- function(functions = "c") {
  checkmate::assert_character(functions, min.chars = 1, any.missing = FALSE, null.ok = TRUE)
  argument_mutator("f(a, b)", "f(b, a)", functions, function(texts) {
    if (identical(rev(texts), texts)) list() else list(rev(texts))
  })
}

# `variants(texts)` returns a list of character vectors, each the argument
# texts for one mutant.
argument_mutator <- function(from, to, functions, variants) {
  node_mutator(
    from = from,
    to = to,
    query = "(call) @target",
    propose = function(node) {
      name <- call_function_name(node)
      if (is.null(name) || name == "return") return(NULL)
      if (!is.null(functions) && !name %in% functions) return(NULL)
      args <- call_argument_nodes(node)
      if (length(args) < 2) return(NULL)
      texts <- vapply(args, treesitter::node_text, character(1))
      args_node <- treesitter::node_child_by_field_name(node, "arguments")
      lapply(variants(texts), function(v) {
        proposal(args_node, paste0("(", paste(v, collapse = ", "), ")"))
      })
    }
  )
}

#' Remove validation calls
#'
#' Replaces calls that enforce preconditions with `NULL`, so
#' `if (!is.numeric(x)) stop("bad")` becomes `if (!is.numeric(x)) NULL`.
#' Surviving mutants mean no test checks that invalid input is rejected. By
#' default targets `stop()`, `stopifnot()`, `warning()`, `rlang::abort()`,
#' `cli::cli_abort()`, `cli::cli_warn()` and checkmate-style `assert_*()` calls.
#'
#' Unlike [delete_statement()], the call is replaced rather than deleted, so
#' calls used as the body of an `if` stay syntactically valid.
#'
#' @param pattern Regular expression matched against the function name.
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_validation()
#' remove_validation("^stopifnot$")
remove_validation <- function(
  pattern = "^(stop|stopifnot|warning|abort|cli_abort|cli_warn|assert_[A-Za-z0-9_]+)$"
) {
  checkmate::assert_string(pattern, min.chars = 1)
  node_mutator(
    from = "stop(...)",
    to = "NULL",
    query = "(call) @target",
    propose = function(node) {
      name <- call_function_name(node)
      if (is.null(name) || !grepl(pattern, name)) return(NULL)
      list(proposal(node, "NULL"))
    }
  )
}

#' Flip logical flags passed as named arguments
#'
#' Flips `TRUE`/`FALSE` only where it is the value of a named argument, e.g.
#' `mean(x, na.rm = TRUE)` becomes `mean(x, na.rm = FALSE)`. A narrower and
#' less noisy alternative to [boolean_literal()] for catching silently-wrong
#' option handling (`na.rm`, `drop`, `fixed`, `decreasing`, ...).
#'
#' @param arguments Character vector of argument names to target. `NULL`, the
#'   default, targets every named argument.
#' @return A [Mutator] object.
#' @export
#' @examples
#' flip_named_flag()
#' flip_named_flag(c("na.rm", "drop"))
flip_named_flag <- function(arguments = NULL) {
  checkmate::assert_character(arguments, min.chars = 1, any.missing = FALSE, null.ok = TRUE)
  node_mutator(
    from = "name = TRUE",
    to = "name = FALSE",
    query = "(argument name: (_) value: [(true) (false)] @target)",
    propose = function(node) {
      name <- field_text(treesitter::node_parent(node), "name")
      if (!is.null(arguments) && !name %in% arguments) return(NULL)
      list(proposal(node, if (treesitter::node_text(node) == "TRUE") "FALSE" else "TRUE"))
    }
  )
}
