#' @import testthat
NULL

#' Base TestStrategy class
#'
#' @md
#' @export
#' @family TestStrategy
TestStrategy <- R6::R6Class(
  classname = "TestStrategy",
  public = list(
    #' @description Execute the test strategy
    #' @param path The path to the test directory
    #' @param mutated_file The path to the file being tested
    #' @param mutated_code The mutated code
    #' @param env The environment to run the tests in
    #' @param reporter The reporter to use for test results
    #' @return The test result
    execute = function(path, mutated_file, mutated_code, env, reporter) {
      stop("Not implemented")
    }
  )
)

#' FullTestStrategy class
#'
#' This class implements a test strategy that runs all tests.
#'
#' @export
#' @family TestStrategy
FullTestStrategy <- R6::R6Class(
  classname = "FullTestStrategy",
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
    #' @param mutated_file The path to the file being tested
    #' @param mutated_code The mutated code
    #' @param env The environment to run the tests in
    #' @param reporter The reporter to use for test results
    #' @return The test results
    execute = function(path, mutated_file, mutated_code, env, reporter) {
      testthat::test_dir(
        path,
        filter = NULL,
        env = env,
        stop_on_failure = FALSE,
        reporter = reporter,
        load_helpers = private$args$load_helpers,
        load_package = private$args$load_package
      )
    }
  )
)

#' FileTestStrategy class
#'
#' This class implements a test strategy that runs tests matching the source file name.
#'
#' @export
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
    #' @param mutated_file The path to the file being tested
    #' @param mutated_code The mutated code
    #' @param env The environment to run the tests in
    #' @param reporter The reporter to use for test results
    #' @return The test results
    execute = function(path, mutated_file, mutated_code, env, reporter) {
      file_name <- tools::file_path_sans_ext(basename(mutated_file))
      if (!any(grepl(file_name, list.files(path)))) {
        return(.empty_test_result())
      }
      testthat::test_dir(
        path = path,
        filter = file_name,
        env = env,
        stop_on_failure = FALSE,
        reporter = reporter,
        load_helpers = private$args$load_helpers,
        load_package = private$args$load_package
      )
    }
  )
)

#' Create a run strategy
#'
#' @param ... Unused, kept for future expansion
#' @return A Test object
#'
#' @export
#' @family TestStrategy
default_test_strategy <- function(...) {
  FileTestStrategy$new(...)
}

.empty_test_result <- function() {
  structure(
    list(),
    class = c("testthat_results")
  )
}
