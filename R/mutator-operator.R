#' Mutate a binary operator
#'
#' Produces one mutant per occurrence of `from` in the source file, replacing
#' it with `to`. A surviving mutant means your tests cannot distinguish the
#' original operator from the replacement — pointing at the missing assertion
#' or input value.
#'
#' Use this when you need a specific swap not covered by the preset collections
#' ([arithmetic_operators()], [comparison_operators()], [logical_operators()]).
#'
#' @param from The operator to replace (e.g. `"+"`, `"=="`, `">"`).
#' @param to The replacement operator.
#' @return A [Mutator] object.
#' @seealso
#'   [comparison_operators()], [arithmetic_operators()], [logical_operators()]
#'   for ready-made preset lists.
#'
#'   `vignette("mutators", package = "muttest")` for the full operator
#'   reference with examples of what each preset catches.
#'
#'   `vignette("interpreting-results", package = "muttest")` to learn how to
#'   read surviving mutants and strengthen the tests they expose.
#' @export
#' @examples
#' operator("+", "-")
#' operator("==", "!=")
#' operator(">", ">=")  # probe the strict vs. non-strict boundary
operator <- function(from, to) {
  checkmate::assert_string(from, min.chars = 1)
  checkmate::assert_string(to, min.chars = 1)
  Mutator$new(
    from = from,
    to = to,
    query = sprintf("(binary_operator
      lhs: (_) @lhs
      operator: _ @target
      rhs: (_) @rhs
      (#eq? @target \"%s\")
    )", from)
  )
}

info_oneline <- function(m) {
  paste(m$from, SYMBOLS$arrow, m$to)
}

replace_with <- function(code, node, replacement_text) {
  start_point <- treesitter::node_start_point(node)
  original_text <- treesitter::node_text(node)
  code[start_point$row + 1] <- paste0(
    substr(code[start_point$row + 1], 1, start_point$column),
    replacement_text,
    substr(
      code[start_point$row + 1],
      start_point$column + nchar(original_text) + 1,
      nchar(code[start_point$row + 1])
    )
  )
  code
}

mutate_code <- function(code, mutator) {
  language <- treesitter.r::language()
  parser <- treesitter::parser(language)
  treesitter_code <- paste(code, collapse = "\n")
  tree <- treesitter::parser_parse(parser, treesitter_code)
  root_node <- treesitter::tree_root_node(tree)

  query <- treesitter::query(language, mutator$query)

  mutations <- list()

  captures <- treesitter::query_captures(query, root_node)

  if (length(captures$node) == 0) {
    return(NULL)
  }

  for (i in seq_along(captures$node)) {
    # By convention all queries name the node to mutate @target. Structural
    # captures (e.g. @lhs, @rhs, @keyword) are present only for predicate
    # filtering and must be skipped here to avoid spurious mutants.
    if (captures$name[[i]] != "target") next
    node <- captures$node[[i]]
    node_text <- treesitter::node_text(node)

    matches_node <- if (!is.null(mutator$match_fn)) {
      mutator$match_fn(node_text)
    } else {
      node_text == mutator$from
    }

    if (!matches_node) next

    replacement <- if (!is.null(mutator$replacement_fn)) {
      mutator$replacement_fn(node_text)
    } else {
      mutator$to
    }

    mutations <- append(mutations, list(replace_with(code, node, replacement)))
  }

  if (length(mutations) == 0) return(NULL)
  mutations
}
