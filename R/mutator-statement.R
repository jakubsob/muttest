delete_node <- function(code, node) {
  start_row <- treesitter::node_start_point(node)$row + 1L
  end_row   <- treesitter::node_end_point(node)$row + 1L
  before    <- if (start_row > 1L) code[seq_len(start_row - 1L)] else character(0)
  after     <- if (end_row < length(code)) code[(end_row + 1L):length(code)] else character(0)
  c(before, after)
}

delete_statements <- function(code, query_str, match_fn) {
  language  <- treesitter.r::language()
  parser    <- treesitter::parser(language)
  ts_code   <- paste(code, collapse = "\n")
  tree      <- treesitter::parser_parse(parser, ts_code)
  root_node <- treesitter::tree_root_node(tree)
  query     <- treesitter::query(language, query_str)
  captures  <- treesitter::query_captures(query, root_node)
  if (length(captures$node) == 0) return(NULL)
  mutations <- list()
  for (i in seq_along(captures$node)) {
    if (captures$name[[i]] != "stmt") next
    node      <- captures$node[[i]]
    node_text <- treesitter::node_text(node)
    if (!match_fn(node_text)) next
    mutations <- append(mutations, list(delete_node(code, node)))
  }
  if (length(mutations) == 0) return(NULL)
  mutations
}

#' Delete statements one at a time
#'
#' Produces one mutant per deletable statement, removing each `x <- expr`
#' assignment or standalone `f(...)` call from the source. Surviving mutants
#' reveal untested side effects or dead assignments.
#'
#' Function definitions (`x <- function(...) { ... }`) are left untouched to
#' avoid producing structurally broken mutants.
#'
#' @return A [Mutator] object.
#' @export
#' @examples
#' delete_statement()
delete_statement <- function() {
  query_str <- paste(
    "[(program (binary_operator operator: _ @op",
    "    (#match? @op \"^(<-|<<-|=)$\")) @stmt)",
    " (braced_expression (binary_operator operator: _ @op",
    "    (#match? @op \"^(<-|<<-|=)$\")) @stmt)",
    " (program (call) @stmt)",
    " (braced_expression (call) @stmt)]"
  )
  match_fn <- function(text) {
    !grepl("^\\s*\\S+\\s*(<-|<<-|=)\\s*function\\s*\\(", text, perl = TRUE)
  }

  Mutator$new(
    from = "<statement>",
    to = "",
    query = query_str,
    match_fn = match_fn,
    mutate_fn = function(code) delete_statements(code, query_str, match_fn)
  )
}
