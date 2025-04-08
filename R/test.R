#' Run a mutation test
#'
#' @param path Path to the test directory.
#' @param source_path Path to the source code directory.
#' @param mutators A list of mutators to use.
#' @param env The environment to use for testing.
#'
#' @importFrom fs dir_copy dir_delete dir_ls
#' @importFrom withr with_dir defer
#' @importFrom purrr map iwalk
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate bind_rows all_of
#' @importFrom testthat test_dir SilentReporter
#' @importFrom cli cli_inform
#' @export
test <- function(
  path,
  source_path = "R",
  mutators = list(operator("+", "-"), operator("*", "/")),
  env = new.env()
) {
  result <- mutators |>
    map(\(mutator) {
      new_path <- paste0("___", source_path, "___")
      dir_copy(source_path, new_path, overwrite = TRUE)
      defer({
        dir_delete(source_path)
        dir_copy(new_path, source_path, overwrite = TRUE)
        dir_delete(new_path)
      })
      cli_inform(c("i" = "Mutating codebase with {info_oneline(mutator)}"))
      dir_ls(source_path, recurse = TRUE, regexp = "*.[rR]$") |>
        map(readLines) |>
        map(mutate_code, mutators = list(mutator)) |>
        iwalk(\(lines, file) {
          writeLines(lines, file)
        })

      cli_inform(c("i" = "Running tests..."))
      result <- test_dir(
        path,
        env = env,
        stop_on_failure = FALSE,
        reporter = SilentReporter$new()
      )
      result |>
        as_tibble() |>
        mutate(
          mutator = list(mutator),
          killed = any(failed > 0)
        )
    }) |>
    bind_rows() |>
    select(all_of(c("file", "test", "failed", "result", "mutator", "killed")))

  total_mutants <- nrow(result)
  killed_mutants <- sum(result$killed)
  score <- (killed_mutants / total_mutants)
  cli_inform(c("v" = "Mutation score: {score * 100}%"))
  score
}
