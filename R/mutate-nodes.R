# Helpers for mutators that need the tree-sitter node (not just its text) to
# decide what to write back, e.g. to reuse a child node's text or to rewrite a
# multi-line span. Mutators built on these set `mutate_fn` on the Mutator.

proposal <- function(node, text) list(node = node, text = text)

# Replace the (possibly multi-line) span of `node` with `text`.
replace_span <- function(code, node, text) {
  s <- treesitter::node_start_point(node)
  e <- treesitter::node_end_point(node)
  first <- code[s$row + 1L]
  last <- code[e$row + 1L]
  new <- strsplit(
    paste0(substr(first, 1, s$column), text, substr(last, e$column + 1L, nchar(last))),
    "\n",
    fixed = TRUE
  )[[1]]
  if (length(new) == 0) new <- ""
  c(code[seq_len(s$row)], new, code[-seq_len(e$row + 1L)])
}

# Run `query_str`, call `propose(node)` for every @target capture and build one
# mutant per returned proposal(). `propose` may return NULL or an empty list.
mutate_nodes <- function(code, query_str, propose) {
  language <- treesitter.r::language()
  parser <- treesitter::parser(language)
  tree <- treesitter::parser_parse(parser, paste(code, collapse = "\n"))
  root_node <- treesitter::tree_root_node(tree)
  captures <- treesitter::query_captures(treesitter::query(language, query_str), root_node)

  mutations <- list()
  for (i in seq_along(captures$node)) {
    if (captures$name[[i]] != "target") next
    for (p in propose(captures$node[[i]])) {
      mutations <- append(mutations, list(list(
        code = replace_span(code, p$node, p$text),
        location = node_location(p$node),
        replacement = p$text
      )))
    }
  }
  if (length(mutations) == 0) return(NULL)
  mutations
}

node_mutator <- function(from, to, query, propose) {
  Mutator$new(
    from = from,
    to = to,
    query = query,
    mutate_fn = function(code) mutate_nodes(code, query, propose)
  )
}

field_text <- function(node, field) {
  child <- treesitter::node_child_by_field_name(node, field)
  if (is.null(child)) NULL else treesitter::node_text(child)
}

# Name of the called function, without any `pkg::` prefix; NULL when the callee
# is not a plain or namespaced identifier.
call_function_name <- function(call_node) {
  fn <- treesitter::node_child_by_field_name(call_node, "function")
  if (is.null(fn)) return(NULL)
  type <- treesitter::node_type(fn)
  if (type == "namespace_operator") return(field_text(fn, "rhs"))
  if (type == "identifier") return(treesitter::node_text(fn))
  NULL
}

# Argument nodes of a call (comments excluded); empty list for `f()`.
call_argument_nodes <- function(call_node) {
  args <- treesitter::node_child_by_field_name(call_node, "arguments")
  if (is.null(args)) return(list())
  Filter(
    function(n) treesitter::node_type(n) == "argument",
    treesitter::node_named_children(args)
  )
}

is_assignment_target <- function(node) {
  parent <- treesitter::node_parent(node)
  if (is.null(parent) || treesitter::node_type(parent) != "binary_operator") {
    return(FALSE)
  }
  op <- field_text(parent, "operator")
  side <- if (op %in% c("<-", "<<-", "=", ":=")) "lhs" else if (op %in% c("->", "->>")) "rhs" else return(FALSE)
  target <- treesitter::node_child_by_field_name(parent, side)
  treesitter::node_start_byte(target) == treesitter::node_start_byte(node) &&
    treesitter::node_end_byte(target) == treesitter::node_end_byte(node)
}
