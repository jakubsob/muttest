Mutator <- R6::R6Class(
  public = list(
    from = NULL,
    to = NULL,
    query = NULL,
    initialize = function(from, to, query) {
      self$from <- from
      self$to <- to
      self$query <- query
    },
    mutate = function(code) {
      mutate_code(code, self)
    },
    # nocov start
    print = function() {
      cat(sprintf("Mutator: %s -> %s\n", self$from, self$to))
      cat(sprintf("Query: %s\n", self$query))
    }
    # nocov end
  )
)

#' Operator mutator
#'
#' It changes a binary operator to another one.
#'
#' @examples
#' operator("==", "!=")
#' operator(">", "<")
#' operator("<", ">")
#' operator("+", "-")
#'
#' @param from The operator to be replaced.
#' @param to The operator to replace with.
#' @export
operator <- function(from, to) {
  Mutator$new(
    from = from,
    to = to,
    query = sprintf('(binary_operator
      lhs: (_) @lhs
      operator: _ @operator
      rhs: (_) @rhs
      (#eq? @operator "%s")
    )', from)
  )
}

info_oneline <- function(m) {
  paste(m$from, cli::symbol$arrow_right, m$to)
}

replace <- function(code, node, mutator) {
  start_point <- treesitter::node_start_point(node)
  code[start_point$row + 1] <- paste0(
    substr(code[start_point$row + 1], 1, start_point$column),
    mutator$to,
    substr(
      code[start_point$row + 1],
      start_point$column + nchar(mutator$from) + 1,
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
  captures <- treesitter::query_captures(query, root_node)

  if (length(captures$node) == 0) {
    return(NULL)
  }

  mutations <- list()
  for (i in seq_along(captures$node)) {
    node <- captures$node[[i]]
    if (treesitter::node_text(node) != mutator$from) {
      next
    }
    mutations <- append(
      mutations,
      list(
        replace(code, node, mutator)
      )
    )
  }

  mutations
}
