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
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr select mutate bind_rows all_of
#' @importFrom testthat test_dir SilentReporter
#' @importFrom cli cli_inform cli_text col_cyan col_red col_green col_yellow
#' @export
test <- function(
  path,
  source_path = "R",
  mutators = list(operator("+", "-"), operator("*", "/")),
  env = new.env()
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
    cli_inform(c(
      "i" = "No mutations were generated. Check your mutators and source files."
    ))
    return(1.0)
  }

  cli_inform(c("i" = "Generated {nrow(plan)} mutations to test"))

  results <- purrr::map(seq_len(nrow(plan)), function(i) {
    row <- plan[i, ]
    mutator <- row$mutator[[1]]
    file_path <- row$file_path
    mutated_code <- row$mutated_code[[1]]

    new_path <- paste0("___", source_path, "___")
    dir_copy(source_path, new_path, overwrite = TRUE)
    defer({
      dir_delete(source_path)
      dir_copy(new_path, source_path, overwrite = TRUE)
      dir_delete(new_path)
    })

    cli_inform(c(
      "i" = "Testing mutation in {file_path} using {mutator$from} -> {mutator$to}"
    ))
    writeLines(mutated_code, file_path)

    test_results <- test_dir(
      path,
      env = env,
      stop_on_failure = FALSE,
      reporter = SilentReporter$new()
    )

    test_results_tibble <- as_tibble(test_results)
    killed <- any(test_results_tibble$failed > 0)

    tibble::tibble(
      file_path = file_path,
      mutator = list(mutator),
      test_results = list(test_results_tibble),
      killed = killed
    )
  }) |>
    dplyr::bind_rows()

  total_mutants <- nrow(results)
  killed_mutants <- sum(results$killed)
  survived_mutants <- total_mutants - killed_mutants
  score <- killed_mutants / total_mutants

  cli::cli_rule(cli::style_bold("Results"))
  cli_text(
    "[ ",
    if (killed_mutants > 0) {
      col_green("KILLED ")
    } else {
      "KILLED "
    },
    killed_mutants,
    " | ",
    if (survived_mutants > 0) {
      col_red("SURVIVED ")
    } else {
      "SURVIVED "
    },
    survived_mutants,
    " | ",
    paste0("TOTAL ", total_mutants),
    " | ",
    col_green("SCORE: "),
    sprintf("%.1f%%", score * 100),
    " ]"
  )

  score
}
