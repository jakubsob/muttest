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
    #' @return A list of mutated code variants (one per match), or `NULL` if
    #'   the pattern was not found.
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
