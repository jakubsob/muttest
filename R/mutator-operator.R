Mutator <- R6::R6Class(
  "Mutator",
  public = list(
    from = NULL,
    to = NULL,
    query = NULL,
    match_fn = NULL,       # function(node_text) -> bool; overrides `node_text == from`
    replacement_fn = NULL, # function(node_text) -> string; overrides `to`
    per_match_fn = NULL,   # function(code, named_captures) -> code|NULL; multi-node mutations
    initialize = function(from, to, query, match_fn = NULL, replacement_fn = NULL, per_match_fn = NULL) {
      self$from <- from
      self$to <- to
      self$query <- query
      self$match_fn <- match_fn
      self$replacement_fn <- replacement_fn
      self$per_match_fn <- per_match_fn
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

#' Mutate an operator
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
    query = sprintf("(binary_operator
      lhs: (_) @lhs
      operator: _ @operator
      rhs: (_) @rhs
      (#eq? @operator \"%s\")
    )", from)
  )
}

info_oneline <- function(m) {
  paste(m$from, cli::symbol$arrow_right, m$to)
}

replace_with <- function(code, node, replacement_text) {
  start_point <- treesitter::node_start_point(node)
  end_point   <- treesitter::node_end_point(node)
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

  if (!is.null(mutator$per_match_fn)) {
    # query_matches returns list[pattern] -> list[match] -> list(name, node)
    pattern_matches <- treesitter::query_matches(query, root_node)
    for (pattern in pattern_matches) {
      for (m in pattern) {
        mutated <- mutator$per_match_fn(code, m)
        if (!is.null(mutated)) mutations <- append(mutations, list(mutated))
      }
    }
    if (length(mutations) == 0) return(NULL)
    return(mutations)
  }

  captures <- treesitter::query_captures(query, root_node)

  if (length(captures$node) == 0) {
    return(NULL)
  }

  for (i in seq_along(captures$node)) {
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
