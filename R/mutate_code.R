#' @import treesitter
replace <- function(code, node, mutator) {
  start <- node_start_byte(node)
  start_point <- node_start_point(node)
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

#' @import treesitter
#' @importFrom treesitter.r language
mutate_code <- function(code, mutator) {
  language <- language()
  parser <- parser(language)
  treesitter_code <- paste(code, collapse = "\n")
  tree <- parser_parse(parser, treesitter_code)
  root_node <- tree_root_node(tree)

  query <- query(language, mutator$query)
  captures <- query_captures(query, root_node)

  if (length(captures$node) == 0) {
    return(NULL)
  }

  mutations <- list()
  for (i in seq_along(captures$node)) {
    node <- captures$node[[i]]
    if (node_text(node) != mutator$from) {
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
