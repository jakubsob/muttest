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
    #' @field per_match_fn Optional `function(code, named_captures)` returning
    #'   mutated code or `NULL` to skip; used for multi-node mutations.
    per_match_fn = NULL,

    #' @description Create a new `Mutator`.
    #' @param from Token to replace.
    #' @param to Replacement token.
    #' @param query Tree-sitter query string.
    #' @param match_fn Optional custom match function.
    #' @param replacement_fn Optional custom replacement function.
    #' @param per_match_fn Optional per-match function for multi-node mutations.
    initialize = function(from, to, query,
                          match_fn = NULL,
                          replacement_fn = NULL,
                          per_match_fn = NULL) {
      self$from <- from
      self$to <- to
      self$query <- query
      self$match_fn <- match_fn
      self$replacement_fn <- replacement_fn
      self$per_match_fn <- per_match_fn
    },

    #' @description Apply this mutator to a character vector of source lines.
    #' @param code Character vector of source lines.
    #' @return A list of mutated code variants (one per match), or `NULL` if
    #'   the pattern was not found.
    mutate = function(code) {
      mutate_code(code, self)
    },

    # nocov start
    #' @description Print a short summary of the mutator.
    print = function() {
      cat(sprintf("Mutator: %s -> %s\n", self$from, self$to))
      cat(sprintf("Query: %s\n", self$query))
    }
    # nocov end
  )
)
