#' Run a mutation test
#'
#' @param path Path to the test directory.
#' @param source_path Path to the source code directory.
#' @param mutators A list of mutators to use.
#' @param env The environment to use for testing.
#' @param reporter Reporter to use for mutation testing results.
#'
#' @importFrom fs dir_copy dir_delete dir_ls file_copy path_rel
#' @importFrom withr with_dir defer local_tempdir
#' @importFrom purrr map iwalk
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr select mutate bind_rows all_of
#' @importFrom testthat test_dir SilentReporter
#' @importFrom cli cli_inform cli_text col_cyan col_red col_green col_yellow
#' @export
test <- function(
  path,
  source_path = "R",
  mutators = list(operator("+", "-"), operator("*", "/")),
  env = new.env(),
  reporter = default_reporter()
) {
  plan <- tibble::tibble()

  for (mutator in mutators) {
    source_files <- fs::dir_ls(source_path, recurse = TRUE, regexp = "*.[rR]$")

    for (file_path in source_files) {
      code_lines <- readLines(file_path)
      mutations <- mutate_code(code_lines, mutator)

      if (is.null(mutations)) {
        next
      }

      for (i in seq_along(mutations)) {
        mutation <- mutations[[i]]
        plan <- dplyr::bind_rows(
          plan,
          tibble::tibble(
            file_path = file_path,
            original_code = list(code_lines),
            mutated_code = list(mutation$code),
            mutator = list(mutator)
          )
        )
      }
    }
  }

  if (nrow(plan) == 0) {
    return(1.0)
  }

  temp_dir <- withr::local_tempdir()
  original_dir <- getwd()

  dirs_to_copy <- list.dirs(original_dir, recursive = FALSE, full.names = FALSE)
  dirs_to_copy <- dirs_to_copy[!grepl("^\\.|tmp|temp", dirs_to_copy)]

  for (dir in dirs_to_copy) {
    if (dir.exists(file.path(original_dir, dir))) {
      fs::dir_copy(
        file.path(original_dir, dir),
        file.path(temp_dir, dir),
        overwrite = TRUE
      )
    }
  }

  reporter$start_reporter(plan, temp_dir)
  withr::with_dir(temp_dir, {
    results <- purrr::map(seq_len(nrow(plan)), function(i) {
      row <- plan[i, ]
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

      test_results <- test_dir(
        path,
        env = env,
        stop_on_failure = FALSE,
        reporter = reporter$test_reporter
      )

      test_results_tibble <- as_tibble(test_results)
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
  sum(results$killed) / nrow(results)
}
