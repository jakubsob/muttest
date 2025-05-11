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
        list(
          code = replace(code, node, mutator),
          mutator = mutator
        )
      )
    )
  }

  mutations
}
