test_that("MultiReporter drives progress and JSON from one run", {
  .with_example_dir("shipping/", {
    # Arrange
    path <- withr::local_tempfile(fileext = ".json")
    mutators <- list(operator(">", "<"), operator(">", ">="))
    plan <- muttest_plan(mutators, fs::dir_ls("R"))
    reporter <- MultiReporter$new(
      ProgressMutationReporter$new(min_time = Inf, survived_detail = "none"),
      JSONMutationReporter$new(path = path)
    )

    # Act
    result <- suppressMessages(muttest(plan, reporter = reporter))

    # Assert
    # JSON child wrote its file
    expect_true(file.exists(path))
    # score comes back and matches the children
    expect_false(is.na(result))
    expect_equal(
      reporter$get_score(),
      reporter$reporters[[2]]$get_score()
    )
  })
})

test_that("MultiReporter rejects non-reporters", {
  expect_error(MultiReporter$new("not a reporter"))
})
