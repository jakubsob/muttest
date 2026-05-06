.with_example_dir <- function(path, code) {
  withr::with_dir(
    system.file("examples", path, package = "muttest"),
    code
  )
}

test_ <- function(...) {
  purrr::quietly(test)(...)$result
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
  },
  variant = ifelse(testthat::is_checking(), "check", "local")
)

test_that("progress reporter shows all killed", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"))
    plan <- plan(mutators, fs::dir_ls("R"))
    .expect_snapshot(
      muttest(
        plan,
        reporter = ProgressMutationReporter$new(
          min_time = Inf,
          survived_detail = "none"
        )
      )
    )
  })
})

test_that("progress reporter shows survived mutants inline", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- plan(mutators, fs::dir_ls("R"))
    .expect_snapshot(
      muttest(
        plan,
        reporter = ProgressMutationReporter$new(
          min_time = Inf,
          survived_detail = "inline"
        )
      )
    )
  })
})

test_that("progress reporter shows survived mutants in summary", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- plan(mutators, fs::dir_ls("R"))
    .expect_snapshot(
      muttest(
        plan,
        reporter = ProgressMutationReporter$new(
          min_time = Inf,
          survived_detail = "summary"
        )
      )
    )
  })
})

test_that("progress reporter shows survived mutants in both", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- plan(mutators, fs::dir_ls("R"))
    .expect_snapshot(
      muttest(
        plan,
        reporter = ProgressMutationReporter$new(
          min_time = Inf,
          survived_detail = "both"
        )
      )
    )
  })
})

test_that("progress reporter shows doesn't show survived mutants", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- plan(mutators, fs::dir_ls("R"))
    .expect_snapshot(
      muttest(
        plan,
        reporter = ProgressMutationReporter$new(
          min_time = Inf,
          survived_detail = "none"
        )
      )
    )
  })
})
