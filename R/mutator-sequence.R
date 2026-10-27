#' Remove one stage from a pipe chain
#'
#' Replaces `x |> f()` (and `x %>% f()`) with `x`, dropping the right-hand
#' stage. In a chain `x |> f() |> g()` each stage is removed in turn. Surviving
#' mutants mean a pipeline step has no observable effect.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' remove_pipe_stage()
remove_pipe_stage <- function() {
  node_mutator(
    from = "x |> f()",
    to = "x",
    query = "(binary_operator) @target",
    propose = function(node) {
      if (!field_text(node, "operator") %in% c("|>", "%>%")) return(NULL)
      list(proposal(node, field_text(node, "lhs")))
    }
  )
}

#' Shrink `seq_len()` by one
#'
#' Replaces `seq_len(n)` with `seq_len(n - 1L)`, catching loops that run one
#' iteration too many or too few.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' seq_len_decrement()
seq_len_decrement <- function() {
  node_mutator(
    from = "seq_len(n)",
    to = "seq_len(n - 1L)",
    query = "(call) @target",
    propose = function(node) {
      if (!identical(call_function_name(node), "seq_len")) return(NULL)
      args <- call_argument_nodes(node)
      if (length(args) != 1) return(NULL)
      args_node <- treesitter::node_child_by_field_name(node, "arguments")
      list(proposal(args_node, paste0("(", treesitter::node_text(args[[1]]), " - 1L)")))
    }
  )
}

#' Skip the first element of `seq_along()`
#'
#' Replaces `seq_along(x)` with `seq_along(x)[-1L]`, catching loops that
#' silently depend on processing the first element.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' seq_along_drop_first()
seq_along_drop_first <- function() {
  node_mutator(
    from = "seq_along(x)",
    to = "seq_along(x)[-1L]",
    query = "(call) @target",
    propose = function(node) {
      if (!identical(call_function_name(node), "seq_along")) return(NULL)
      list(proposal(node, paste0(treesitter::node_text(node), "[-1L]")))
    }
  )
}

#' Shift the bounds of `a:b` ranges
#'
#' Replaces the start of a range with `(a + 1L)` ([colon_start_increment()]) or
#' the end with `(b - 1L)` ([colon_end_decrement()]), the classic off-by-one in
#' `1:n` loops. Only non-literal bounds are touched; literal bounds are already
#' covered by [numeric_increment()] and [numeric_decrement()].
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' colon_start_increment()
colon_start_increment <- function() {
  colon_mutator("(a + 1L):b", "lhs", " + 1L")
}

#' @rdname colon_start_increment
#' @export
#' @examples
#' colon_end_decrement()
colon_end_decrement <- function() {
  colon_mutator("a:(b - 1L)", "rhs", " - 1L")
}

colon_mutator <- function(to, side, shift) {
  node_mutator(
    from = "a:b",
    to = to,
    query = "(binary_operator) @target",
    propose = function(node) {
      if (field_text(node, "operator") != ":") return(NULL)
      bound <- treesitter::node_child_by_field_name(node, side)
      if (treesitter::node_type(bound) %in% c("float", "integer")) return(NULL)
      list(proposal(bound, paste0("(", treesitter::node_text(bound), shift, ")")))
    }
  )
}
