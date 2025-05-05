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
    #' @param file_path The path to the file being tested
    #' @param mutated_code The mutated code
    #' @param env The environment to run the tests in
    #' @param reporter The reporter to use for test results
    #' @return The test result
    execute = function(path, file_path, mutated_code, env, reporter) {
      stop("Method not implemented for base Test class")
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
  public = list(
    #' @description Execute the test strategy
    #' @param path The path to the test directory
    #' @param file_path The path to the file being tested
    #' @param mutated_code The mutated code
    #' @param env The environment to run the tests in
    #' @param reporter The reporter to use for test results
    #' @return The test results
    execute = function(path, file_path, mutated_code, env, reporter) {
      testthat::test_dir(
        path,
        env = env,
        stop_on_failure = FALSE,
        reporter = reporter
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
  public = list(
    #' @description Execute the test strategy
    #' @param path The path to the test directory
    #' @param file_path The path to the file being tested
    #' @param mutated_code The mutated code
    #' @param env The environment to run the tests in
    #' @param reporter The reporter to use for test results
    #' @return The test results
    execute = function(path, file_path, mutated_code, env, reporter) {
      file_name <- tools::file_path_sans_ext(basename(file_path))
      if (!any(grepl(file_name, list.files(path)))) {
        file_name <- NULL
      }
      testthat::test_dir(
        path,
        filter = file_name,
        env = env,
        stop_on_failure = FALSE,
        reporter = reporter
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
