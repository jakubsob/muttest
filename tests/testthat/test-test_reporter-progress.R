test_that("progress reporter shows all killed", {
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"))
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
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"), operator(">", ">="))
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
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"), operator(">", ">="))
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
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"), operator(">", ">="))
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

test_that("progress reporter doesn't show survived mutant details", {
  .with_example_dir("shipping/", {
    mutators <- list(operator(">", "<"), operator(">", ">="))
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
