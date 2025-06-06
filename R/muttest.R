#' Run a mutation test
#'
#' @param plan A data frame with the test plan. See `plan()`.
#' @param path Path to the test directory.
#' @param reporter Reporter to use for mutation testing results. See `?MutationReporter`.
#' @param test_strategy Strategy for running tests. See `?TestStrategy`.
#'   The purpose of test strategy is to control how tests are executed.
#'   We can run all tests for each mutant, or only tests that are relevant to the mutant.
#' @param copy_strategy Strategy for copying the project. See `?CopyStrategy`.
#'   This strategy controls which files are copied to the temporary directory, where the tests are run.
#'
#' @return A numeric value representing the mutation score.
#'
#' @export
#' @md
#' @importFrom rlang .data
muttest <- function(
  plan,
  path = "tests/testthat",
  reporter = default_reporter(),
  test_strategy = default_test_strategy(),
  copy_strategy = default_copy_strategy()
) {
  checkmate::assert_directory_exists(path)
  checkmate::assert(
    checkmate::check_data_frame(plan),
    checkmate::check_set_equal(
      c("filename", "original_code", "mutated_code", "mutator"),
      names(plan)
    ),
    combine = "and"
  )
  checkmate::assert_class(reporter, "MutationReporter")
  checkmate::assert_class(test_strategy, "TestStrategy", null.ok = TRUE)
  checkmate::assert_class(copy_strategy, "CopyStrategy")

  if (nrow(plan) == 0) {
    return(invisible(NA_real_))
  }

  reporter$start_reporter(plan)

  plan |>
    dplyr::arrange(.data$filename, .data$mutator) |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    purrr::walk(\(row) {
      mutator <- row$mutator[[1]]
      filename <- row$filename
      mutated_code <- row$mutated_code[[1]]

      reporter$start_file(filename)
      reporter$start_mutator(mutator)
      reporter$update(force = TRUE)

      dir <- copy_strategy$execute(getwd(), row)
      checkmate::assert_directory_exists(dir)
      on.exit(fs::dir_delete(dir))
      withr::with_tempdir(tmpdir = dir, pattern = "", {
        withr::with_dir(dir, {
          temp_filename <- file.path(dir, filename)
          writeLines(mutated_code, temp_filename)

          test_results <- test_strategy$execute(
            path = path,
            plan = row,
            reporter = reporter$test_reporter
          )
          checkmate::assert_class(test_results, "testthat_results")
        })
      })

      test_results_tibble <- tibble::as_tibble(test_results)
      killed <- as.numeric(sum(test_results_tibble$failed) > 0)
      survived <- as.numeric(sum(test_results_tibble$failed) == 0)
      errors <- sum(test_results_tibble$error)

      reporter$add_result(
        row,
        killed,
        survived,
        errors
      )
      reporter$end_mutator()
      reporter$end_file()
    })

  reporter$end_reporter()
  invisible(reporter$get_score())
}

#' Create a plan for mutation testing
#'
#' Each mutant requires rerunning the tests. For large project it might be not feasible to test all
#' mutants in one go. This function allows you to create a plan for selected source files and mutators.
#'
#' The plan is in a data frame format, where each row represents a mutant.
#'
#' You can subset the plan before passing it to the `muttest()` function.
#'
#' @param mutators A list of mutators to use. See [operator()].
#' @param source_files A vector of file paths to the source files.
#' @return A data frame with the test plan.
#'   The data frame has the following columns:
#'   - `filename`: The name of the source file.
#'   - `original_code`: The original code of the source file.
#'   - `mutated_code`: The mutated code of the source file.
#'   - `mutator`: The mutator that was applied.
#'
#' @export
#' @md
plan <- function(
  mutators,
  source_files = fs::dir_ls("R", regexp = ".[rR]$")
) {
  checkmate::assert_file_exists(source_files, extension = c("R", "r"))
  checkmate::assert_list(mutators)
  map_dfr <- purrr::compose(dplyr::bind_rows, purrr::map)
  map_dfr(mutators, function(mutator) {
    map_dfr(source_files, function(filename) {
      code_lines <- readLines(filename)
      mutations <- mutator$mutate(code_lines)
      if (length(mutations) == 0) {
        return(
          tibble::tibble(
            filename = character(),
            original_code = list(character()),
            mutated_code = list(character()),
            mutator = list(mutator)
          )
        )
      }
      map_dfr(mutations, function(mutation) {
        tibble::tibble(
          filename = filename,
          original_code = list(code_lines),
          mutated_code = list(mutation),
          mutator = list(mutator)
        )
      })
    })
  })
}
