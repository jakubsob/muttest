test_that("progress reporter shows all killed", {
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"))
    plan <- muttest_plan(mutators, fs::dir_ls("R"))
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

test_that("progress reporter shows survived mutants in summary", {
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"), operator(">", ">="))
    plan <- muttest_plan(mutators, fs::dir_ls("R"))
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

test_that("progress reporter doesn't show survived mutant details", {
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"), operator(">", ">="))
    plan <- muttest_plan(mutators, fs::dir_ls("R"))
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

test_that("survived diff handles mutants that change the line count", {
  out_file <- withr::local_tempfile()
  reporter <- ProgressMutationReporter$new(min_time = Inf, file = out_file)
  reporter$.__enclos_env__$private$print_survived_diff(
    c("a", "b", "c", "d"),
    c("a", "c", "d"),
    "f.R",
    list(from = "<statement>", to = "")
  )
  out <- readLines(out_file)
  expect_match(out, "2- b", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("+", out, fixed = TRUE)))
})
