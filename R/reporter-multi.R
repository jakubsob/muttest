#' @title Combine Several Mutation Reporters
#'
#' @description
#' Fans out every reporter event to a list of child reporters, so a single
#' [muttest()] run can drive more than one reporter at once -- for example a
#' live [ProgressMutationReporter] alongside a [JSONMutationReporter] that
#' writes a machine-readable artifact.
#'
#' @field reporters List of child reporters.
#'
#' @md
#' @export
#' @family MutationReporter
MultiReporter <- R6::R6Class(
  classname = "MultiReporter",
  inherit = MutationReporter,
  public = list(
    reporters = NULL,

    #' @description Initialize a combined reporter
    #' @param ... One or more [MutationReporter] objects.
    initialize = function(...) {
      self$reporters <- list(...)
      checkmate::assert_list(
        self$reporters,
        types = "MutationReporter",
        min.len = 1
      )
      # Tests run inside daemons use this; take it from the first child.
      self$test_reporter <- self$reporters[[1]]$test_reporter
    },

    #' @description Start reporter
    #' @param plan The complete mutation plan
    start_reporter = function(plan = NULL) {
      private$each(function(r) r$start_reporter(plan))
    },

    #' @description Start testing a file
    #' @param filename Path to the file being mutated
    start_file = function(filename) {
      private$each(function(r) r$start_file(filename))
    },

    #' @description Start testing with a specific mutator
    #' @param mutator The mutator being applied
    start_mutator = function(mutator) {
      private$each(function(r) r$start_mutator(mutator))
    },

    #' @description Add a mutation test result
    #' @param ... Arguments forwarded to each child's `add_result()`.
    add_result = function(...) {
      args <- list(...)
      private$each(function(r) do.call(r$add_result, args))
    },

    #' @description Update status
    #' @param force Passed to each child
    update = function(force = FALSE) {
      private$each(function(r) r$update(force))
    },

    #' @description End testing with current mutator
    end_mutator = function() {
      private$each(function(r) r$end_mutator())
    },

    #' @description End testing current file
    end_file = function() {
      private$each(function(r) r$end_file())
    },

    #' @description End reporter
    end_reporter = function() {
      private$each(function(r) r$end_reporter())
    },

    #' @description Get the score (from the first child)
    get_score = function() {
      self$reporters[[1]]$get_score()
    },

    #' @description Print each child that supports printing
    print = function() {
      private$each(function(r) if (is.function(r$print)) r$print())
      invisible(self)
    }
  ),
  private = list(
    each = function(fn) {
      for (r in self$reporters) fn(r)
      invisible(NULL)
    }
  )
)
