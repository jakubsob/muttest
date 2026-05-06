#' @import testthat
NULL

#' @title TestStrategy interface
#'
#' @description
#' Extend this class to implement a custom test strategy.
#'
#' @export
#' @md
#' @family TestStrategy
TestStrategy <- R6::R6Class(
  classname = "TestStrategy",
  public = list(
    #' @description Execute the test strategy
    #' @param path The path to the test directory
    #' @param plan The current mutation plan. See `plan()`.
    #' @param reporter The reporter to use for test results
    #' @return The test result
    execute = function(path, plan, reporter) {
      stop("Not implemented")
    }
  )
)

#' @title Run all tests for a mutant
#'
#' @description
#' This test strategy tells if a mutant is caught by any test.
#'
#' To get faster results, especially for big codebases, use `?FileTestStrategy` instead.
#'
#' @export
#' @md
#' @family TestStrategy
FullTestStrategy <- R6::R6Class(
  classname = "FullTestStrategy",
  inherit = TestStrategy,
  private = list(
    args = list()
  ),
  public = list(
    #' @description Initialize
    #' @param load_helpers Whether to load test helpers
    #' @param load_package The package loading strategy
    initialize = function(
      load_helpers = TRUE,
      load_package = c("source", "none", "installed")
    ) {
      private$args <- list(
        load_helpers = load_helpers,
        load_package = load_package
      )
    },
    #' @description Execute the test strategy
    #' @param path The path to the test directory
    #' @param plan The current mutation plan. See `plan()`.
    #' @param reporter The reporter to use for test results
    #' @return The test results
    execute = function(path, plan, reporter) {
      testthat::test_dir(
        path,
        stop_on_failure = FALSE,
        reporter = reporter,
        load_helpers = private$args$load_helpers,
        load_package = private$args$load_package
      )
    }
  )
)

#' @title Run tests matching the mutated source file name
#'
#' @description
#' This strategy tells if a mutant is caught by a test matching the source file name.
#'
#' For example, if the source file name is `foo.R`, and there are test files named `test-foo.R` or `test-bar.R`,
#' only `test-foo.R` will be run.
#'
#' This strategy should give faster results than `?FullTestStrategy`, especially for big codebases,
#' but the score might be less accurate.
#'
#' @export
#' @md
#' @family TestStrategy
FileTestStrategy <- R6::R6Class(
  classname = "FileTestStrategy",
  inherit = TestStrategy,
  private = list(
    args = list()
  ),
  public = list(
    #' @description Initialize the FileTestStrategy
    #' @param load_helpers Whether to load test helpers
    #' @param load_package The package loading strategy
    initialize = function(
      load_helpers = TRUE,
      load_package = c("source", "none", "installed")
    ) {
      private$args <- list(
        load_helpers = load_helpers,
        load_package = load_package
      )
    },
    #' @description Execute the test strategy
    #' @param path The path to the test directory
    #' @param plan The current mutation plan. See `plan()`.
    #' @param reporter The reporter to use for test results
    #' @return The test results
    execute = function(path, plan, reporter) {
      file_name <- tools::file_path_sans_ext(basename(plan$filename))
      if (!any(grepl(file_name, list.files(path)))) {
        return(.empty_test_result())
      }
      testthat::test_dir(
        path = path,
        filter = file_name,
        stop_on_failure = FALSE,
        reporter = reporter,
        load_helpers = private$args$load_helpers,
        load_package = private$args$load_package
      )
    }
  )
)

#' @title Create a default run strategy
#'
#' @param ... Arguments passed to the `?FullTestStrategy` constructor.
#' @return A `?TestStrategy` object
#'
#' @export
#' @md
#' @family TestStrategy
default_test_strategy <- function(...) {
  FullTestStrategy$new(...)
}

.empty_test_result <- function() {
  structure(
    list(),
    class = c("testthat_results")
  )
}
