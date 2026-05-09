.with_example_dir <- function(path, code) {
  withr::with_dir(
    system.file("examples", path, package = "muttest"),
    code
  )
}

.muttest <- function(...) {
  purrr::quietly(muttest)(...)$result
}

.expect_snapshot <- purrr::partial(
  testthat::expect_snapshot,
  transform = function(lines) {
    lines |>
      stringr::str_subset("^[\\|/\\-\\\\] \\|", negate = TRUE) |>
      stringr::str_subset("^$", negate = TRUE) |>
      stringr::str_remove_all("\\s\\[\\d+.\\d+s\\]") |>
      stringr::str_remove_all("Duration:\\s\\d+.\\d+\\ss") |>
      stringr::str_trim()
  }
)
