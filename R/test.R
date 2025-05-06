#' Run a mutation test
#'
#' @param path Path to the test directory.
#' @param plan A data frame with the test plan. See [test_plan()].
#' @param reporter Reporter to use for mutation testing results.
#' @param test_strategy Strategy for running tests.
#' @param source_loader Function to load source files.
#' @param project_copy_stragtegy Strategy for copying the project.
#' @return A numeric value representing the mutation score.
#'
#' @export
test <- function(
  path,
  plan,
  reporter = default_reporter(),
  test_strategy = default_test_strategy(),
  source_loader = default_source_loader(),
  project_copy_stragtegy = default_project_copy_strategy()
) {
  checkmate::assert_directory_exists(path)
  checkmate::assert(
    checkmate::check_data_frame(plan),
    checkmate::check_set_equal(
      c("file_path", "original_code", "mutated_code", "mutator"),
      names(plan)
    ),
    combine = "and"
  )
  checkmate::assert_class(reporter, "MutationReporter")
  checkmate::assert_class(test_strategy, "TestStrategy", null.ok = TRUE)
  checkmate::assert_function(source_loader, nargs = 1)
  checkmate::assert_class(project_copy_stragtegy, "ProjectCopyStrategy")

  if (nrow(plan) == 0) {
    return(invisible(NA_real_))
  }

  reporter$start_reporter(plan)

  results <- plan |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    purrr::map(\(row) {
      mutator <- row$mutator[[1]]
      file_path <- row$file_path
      original_code <- row$original_code[[1]]
      mutated_code <- row$mutated_code[[1]]

      reporter$start_file(file_path)
      reporter$start_mutator(mutator)
      reporter$update(force = TRUE)

      dir <- project_copy_stragtegy$execute(getwd(), row)
      checkmate::assert_directory_exists(dir)

      withr::with_tempdir(tmpdir = dir, pattern = "", {
        withr::with_dir(dir, {
          temp_file_path <- file.path(dir, file_path)
          writeLines(mutated_code, temp_file_path)

          env <- source_loader(dir)
          checkmate::assert_environment(env)

          test_results <- test_strategy$execute(
            path = path,
            mutated_file = file_path,
            mutated_code = mutated_code,
            env = env,
            reporter = reporter$test_reporter
          )
          checkmate::assert_class(test_results, "testthat_results")
        })
      })

      test_results_tibble <- tibble::as_tibble(test_results)
      killed <- any(test_results_tibble$failed > 0)

      reporter$add_result(file_path, mutator, test_results_tibble, killed)
      reporter$end_mutator()
      reporter$end_file()

      tibble::tibble(
        file_path = file_path,
        mutator = list(mutator),
        test_results = list(test_results_tibble),
        killed = killed
      )
    }) |>
    dplyr::bind_rows()

  reporter$end_reporter()
  invisible(sum(results$killed) / nrow(results))
}

#' Create a test plan for mutation testing
#'
#' @param source_files A vector of file paths to the source files.
#' @param mutators A list of mutators to use.
#' @return A data frame with the test plan.
#' @export
test_plan <- function(source_files, mutators) {
  checkmate::assert_file_exists(source_files, extension = c("R", "r"))
  checkmate::assert_list(mutators)
  map_dfr <- purrr::compose(dplyr::bind_rows, purrr::map)
  map_dfr(mutators, function(mutator) {
    map_dfr(source_files, function(file_path) {
      code_lines <- readLines(file_path)
      mutations <- mutate_code(code_lines, mutator)
      if (is.null(mutations)) {
        return(
          tibble::tibble(
            file_path = character(),
            original_code = list(character()),
            mutated_code = list(character()),
            mutator = list(mutator)
          )
        )
      }
      map_dfr(mutations, function(mutation) {
        tibble::tibble(
          file_path = file_path,
          original_code = list(code_lines),
          mutated_code = list(mutation$code),
          mutator = list(mutator)
        )
      })
    })
  })
}
