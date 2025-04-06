#' @import treesitter
replace <- function(code, node, mutator) {
  if (node_text(node) == mutator$from) {
    start <- node_start_byte(node)
    start_point <- node_start_point(node)
    code[start_point$row + 1] <- paste0(
      substr(code[start_point$row + 1], 1, start_point$column),
      mutator$to,
      substr(code[start_point$row + 1], start_point$column + nchar(mutator$from) + 1, nchar(code[start_point$row + 1]))
    )
  }
  code
}

#' @import treesitter
#' @importFrom treesitter.r language
mutate_code <- function(code, mutators) {
  language <- language()
  parser <- parser(language)
  treesitter_code <- paste(code, collapse = "\n")
  tree <- parser_parse(parser, treesitter_code)
  root_node <- tree_root_node(tree)
  for (mutator in mutators) {
    query <- query(language, mutator$query)
    captures <- query_captures(query, root_node)
    for (node in captures$node) {
      code <- replace(code, node, mutator)
    }
  }
  code
}
