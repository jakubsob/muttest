#' Mutator
#'
#' Represents a single code mutation — a pattern to find and a replacement to
#' apply. Every mutator function ([operator()], [boolean_literal()], etc.)
#' returns an instance of this class.
#'
#' @export
Mutator <- R6::R6Class(
  "Mutator",
  public = list(
    #' @field from The token or operator to replace.
    from = NULL,
    #' @field to The replacement token or operator.
    to = NULL,
    #' @field query Tree-sitter query used to locate candidate nodes.
    query = NULL,
    #' @field match_fn Optional `function(node_text)` returning `logical`;
    #'   overrides the default `node_text == from` equality check.
    match_fn = NULL,
    #' @field replacement_fn Optional `function(node_text)` returning a string;
    #'   overrides the static `to` value as the replacement text.
    replacement_fn = NULL,
    #' @field mutate_fn Optional `function(code)` that fully replaces the
    #'   default mutation logic when set.
    mutate_fn = NULL,

    #' @description Create a new `Mutator`.
    #' @param from Token to replace.
    #' @param to Replacement token.
    #' @param query Tree-sitter query string.
    #' @param match_fn Optional custom match function.
    #' @param replacement_fn Optional custom replacement function.
    #' @param mutate_fn Optional `function(code)` that fully overrides the
    #'   default mutation logic.
    initialize = function(
      from,
      to,
      query,
      match_fn = NULL,
      replacement_fn = NULL,
      mutate_fn = NULL
    ) {
      self$from <- from
      self$to <- to
      self$query <- query
      self$match_fn <- match_fn
      self$replacement_fn <- replacement_fn
      self$mutate_fn <- mutate_fn
    },

    #' @description Apply this mutator to a character vector of source lines.
    #' @param code Character vector of source lines.
    #' @return A list of mutation records (one per match), each a list with
    #'   `code` (mutated source lines), `location` (1-based `start`/`end`
    #'   line/column), and `replacement` (the inserted text), or `NULL` if the
    #'   pattern was not found.
    mutate = function(code) {
      if (!is.null(self$mutate_fn)) {
        self$mutate_fn(code)
      } else {
        mutate_code(code, self)
      }
    },

    # nocov start
    #' @description Print a short summary of the mutator.
    print = function() {
      cat(sprintf("Mutator: %s %s %s\n", self$from, SYMBOLS$arrow, self$to))
      cat(sprintf("Query: %s\n", self$query))
    }
    # nocov end
  )
)


info_oneline <- function(m) {
  paste(m$from, SYMBOLS$arrow, m$to)
}

# 1-based, end-exclusive location of a treesitter node (points are 0-based).
node_location <- function(node) {
  s <- treesitter::node_start_point(node)
  e <- treesitter::node_end_point(node)
  list(
    start = list(line = s$row + 1L, column = s$column + 1L),
    end = list(line = e$row + 1L, column = e$column + 1L)
  )
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
  list(code = code, location = node_location(node), replacement = replacement_text)
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
