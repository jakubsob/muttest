#' Run a mutation test
#'
#' @param path Path to the test directory.
#' @param source_path Path to the source code directory.
#' @param mutators A list of mutators to use.
#' @param env The environment to use for testing.
#' @param reporter Reporter to use for mutation testing results.
#' @param plan A data frame with the test plan. See [test_plan()].
#' @param test_strategy Strategy for running tests: "full" (all tests) or "file" (tests matching source file name).
#'   Can also be a RunStrategy object. See [create_run_strategy()].
#'
#' @export
test <- function(
  path,
  mutators,
  source_path = "R",
  env = new.env(),
  reporter = default_reporter(),
  plan = test_plan(
    fs::dir_ls(source_path, recurse = TRUE, regexp = "*.[rR]$"),
    mutators
  ),
  test_strategy = default_test_strategy()
) {
  checkmate::assert_directory_exists(path)
  checkmate::assert_directory_exists(source_path)
  checkmate::assert_list(mutators)
  checkmate::assert_environment(env)
  checkmate::assert_class(reporter, "MutationReporter")
  checkmate::assert(
    checkmate::check_data_frame(plan),
    checkmate::check_set_equal(
      c("file_path", "original_code", "mutated_code", "mutator"),
      names(plan)
    ),
    combine = "and"
  )
  checkmate::assert_class(test_strategy, "TestStrategy", null.ok = TRUE)

  if (nrow(plan) == 0) {
    return(invisible(1.0))
  }

  temp_dir <- withr::local_tempdir()
  original_dir <- getwd()

  # Get directories to copy (exclude hidden, temp directories)
  dirs_to_copy <- list.dirs(original_dir, recursive = FALSE, full.names = FALSE)
  dirs_to_copy <- dirs_to_copy[!grepl("^\\.|tmp|temp", dirs_to_copy)]

  # Use purrr::walk to copy directories
  purrr::walk(dirs_to_copy, function(dir) {
    src_path <- file.path(original_dir, dir)
    if (dir.exists(src_path)) {
      fs::dir_copy(
        src_path,
        file.path(temp_dir, dir),
        overwrite = TRUE
      )
    }
  })

  reporter$start_reporter(plan, temp_dir)
  withr::with_dir(temp_dir, {
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

        temp_file_path <- file.path(temp_dir, file_path)
        withr::defer(writeLines(original_code, temp_file_path))
        writeLines(mutated_code, temp_file_path)

        # Use the run method of R6 strategy object
        test_results <- test_strategy$execute(
          path = path,
          file_path = file_path,
          mutated_code = mutated_code,
          env = env,
          reporter = reporter$test_reporter
        )

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
  })

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
