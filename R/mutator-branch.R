#' Remove the else branch of if statements
#'
#' Turns `if (c) a else b` into `if (c) a`. Surviving mutants mean no test
#' exercises the `else` path with an observable result.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_else_branch()
remove_else_branch <- function() {
  node_mutator(
    from = "if (c) a else b",
    to = "if (c) a",
    query = "(if_statement alternative: (_)) @target",
    propose = function(node) {
      consequence <- treesitter::node_child_by_field_name(node, "consequence")
      keep <- treesitter::node_end_byte(consequence) - treesitter::node_start_byte(node)
      text <- rawToChar(charToRaw(treesitter::node_text(node))[seq_len(keep)])
      list(proposal(node, text))
    }
  )
}

#' Empty the body of if statements
#'
#' Replaces the consequence of each `if` with an empty block, so
#' `if (c) a else b` becomes `if (c) {} else b`. Surviving mutants mean the
#' effect of the `if` body is never observed by a test.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' empty_if_body()
empty_if_body <- function() {
  node_mutator(
    from = "if (c) a",
    to = "if (c) {}",
    query = "(if_statement consequence: (_) @target)",
    propose = function(node) {
      if (grepl("^\\{\\s*\\}$", treesitter::node_text(node))) return(NULL)
      list(proposal(node, "{}"))
    }
  )
}

#' Remove `is.null()` guard clauses
#'
#' Replaces an early-return guard such as `if (is.null(x)) return(default)` with
#' `NULL`, so the function carries on as if the guard never fired. Only `if`
#' statements without an `else` whose body is a `return()` call are targeted.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_null_guard()
remove_null_guard <- function() {
  is_return <- function(node) {
    treesitter::node_type(node) == "call" && identical(call_function_name(node), "return")
  }
  node_mutator(
    from = "if (is.null(x)) return(y)",
    to = "NULL",
    query = "(if_statement) @target",
    propose = function(node) {
      if (!is.null(treesitter::node_child_by_field_name(node, "alternative"))) return(NULL)
      condition <- treesitter::node_child_by_field_name(node, "condition")
      if (!grepl("is.null(", treesitter::node_text(condition), fixed = TRUE)) return(NULL)
      body <- treesitter::node_child_by_field_name(node, "consequence")
      if (treesitter::node_type(body) == "braced_expression") {
        stmts <- Filter(
          function(n) treesitter::node_type(n) != "comment",
          treesitter::node_named_children(body)
        )
        if (length(stmts) != 1) return(NULL)
        body <- stmts[[1]]
      }
      if (!is_return(body)) return(NULL)
      list(proposal(node, "NULL"))
    }
  )
}
