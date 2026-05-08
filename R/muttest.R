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
#' @param workers Number of parallel workers. When greater than 1, mutants are tested
#'   concurrently using `mirai` daemons. Defaults to 1 (sequential).
#' @param timeout Per-mutant timeout in milliseconds. If a mutant's test run exceeds this
#'   limit the daemon is interrupted and the result is recorded as an error.
#'   Use `Inf` to disable. Defaults to 600000 (10 minutes).
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
  copy_strategy = default_copy_strategy(),
  workers = 1,
  timeout = 600 * 1000
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
  checkmate::assert_count(workers, positive = TRUE)
  checkmate::assert_number(timeout, lower = 0)

  if (nrow(plan) == 0) {
    return(invisible(NA_real_))
  }

  reporter$start_reporter(plan)

  rows <- plan |>
    dplyr::arrange(.data$filename, .data$mutator) |>
    dplyr::rowwise() |>
    dplyr::group_split()

  mirai::daemons(workers)
  on.exit(mirai::daemons(0), add = TRUE)

  wd <- getwd()
  test_reporter <- reporter$test_reporter
  timeout_ms <- if (is.finite(timeout)) as.integer(timeout) else NULL

  tasks <- lapply(rows, function(row) {
    reporter$start_file(row$filename)
    reporter$start_mutator(row$mutator[[1]])
    reporter$update(force = TRUE)

    filename <- row$filename
    mutated_code <- row$mutated_code[[1]]

    # Pass only plain serializable data — row$mutator contains treesitter
    # C-level objects that cannot cross process boundaries.
    mirai::mirai(
      {
        # Reconstruct a minimal row so strategies can access $filename
        minimal_row <- tibble::tibble(
          filename = filename,
          mutated_code = list(mutated_code)
        )
        dir <- copy_strategy$execute(wd, minimal_row)
        on.exit(fs::dir_delete(dir))
        test_results <- tryCatch(
          withr::with_tempdir(tmpdir = dir, pattern = "", {
            withr::with_dir(dir, {
              writeLines(mutated_code, file.path(dir, filename))
              test_strategy$execute(path, minimal_row, test_reporter)
            })
          }),
          error = function(e) e
        )
        list(test_results = test_results)
      },
      copy_strategy = copy_strategy,
      test_strategy = test_strategy,
      wd = wd,
      filename = filename,
      mutated_code = mutated_code,
      path = path,
      test_reporter = test_reporter,
      .timeout = timeout_ms
    )
  })

  for (i in seq_along(tasks)) {
    res <- tasks[[i]][] # blocks until the task resolves
    row <- rows[[i]]
    if (mirai::is_error_value(res) && !mirai::is_mirai_error(res)) {
      .record_result(
        reporter,
        row,
        simpleError("Timed out"),
        row$mutated_code[[1]]
      )
    } else if (mirai::is_error_value(res)) {
      .record_result(
        reporter,
        row,
        simpleError(format(res)),
        row$mutated_code[[1]]
      )
    } else {
      .record_result(reporter, row, res$test_results, row$mutated_code[[1]])
    }
  }

  reporter$end_reporter()
  invisible(reporter$get_score())
}

.record_result <- function(reporter, row, test_results, mutated_code) {
  if (inherits(test_results, "error")) {
    reporter$add_result(
      row,
      killed = 0,
      survived = 0,
      errors = 1,
      error = test_results,
      original_code = row$original_code[[1]],
      mutated_code = mutated_code
    )
  } else {
    tib <- tibble::as_tibble(test_results)
    reporter$add_result(
      row,
      killed = as.numeric(sum(tib$failed) > 0),
      survived = as.numeric(sum(tib$failed) == 0),
      errors = sum(tib$error),
      original_code = row$original_code[[1]],
      mutated_code = mutated_code
    )
  }
  reporter$end_mutator()
  reporter$end_file()
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
