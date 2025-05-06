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
    # nocov start
    print = function() {
      cat(sprintf("Mutator: %s -> %s\n", self$from, self$to))
      cat(sprintf("Query: %s\n", self$query))
    }
    # nocov end
  )
)

#' Operator mutator
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
