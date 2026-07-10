test_that("JSON reporter emits the mutation-testing-elements schema", {
  .with_example_dir("shipping/", {
    # Arrange
    path <- withr::local_tempfile(fileext = ".json")
    mutators <- list(operator(">", "<"), operator(">", ">="))
    plan <- muttest_plan(mutators, fs::dir_ls("R"))

    # Act
    suppressMessages(muttest(
      plan,
      reporter = JSONMutationReporter$new(path = path)
    ))

    # Assert
    expect_snapshot_file(path, "muttest-report.json")
  })
})

test_that("JSON output conforms to the mutation-testing-elements schema", {
  # Arrange
  schema <- system.file(
    "schema",
    "mutation-testing-report-schema.json",
    package = "muttest"
  )
  .with_example_dir("shipping/", {
    path <- withr::local_tempfile(fileext = ".json")
    plan <- muttest_plan(list(operator(">", "<")), fs::dir_ls("R"))

    # Act
    suppressMessages(muttest(
      plan,
      reporter = JSONMutationReporter$new(path = path)
    ))

    # Assert
    expect_true(jsonvalidate::json_validate(
      path,
      schema,
      engine = "ajv",
      verbose = TRUE
    ))
  })
})
