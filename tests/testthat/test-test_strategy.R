describe("TestStrategy", {
  it("throws error when calling execute()", {
    # Arrange
    strategy <- TestStrategy$new()

    # Act, Assert
    expect_error(
      strategy$execute("path", "file.R", "code", new.env(), NULL),
      "Not implemented"
    )
  })
})

describe("FullTestStrategy", {
  it("executes all tests in the directory", {
    # Arrange
    temp_dir <- withr::local_tempdir()
    test_dir <- file.path(temp_dir, "tests/testthat")
    dir.create(test_dir, recursive = TRUE)
    test1 <- 'test_that("test1", { expect_true(TRUE) })'
    test2 <- 'test_that("test2", { expect_true(TRUE) })'
    writeLines(test1, file.path(test_dir, "test-file1.R"))
    writeLines(test2, file.path(test_dir, "test-file2.R"))
    strategy <- FullTestStrategy$new(load_package = "none")

    # Act
    result <- strategy$execute(
      path = test_dir,
      mutated_file = "file.R",
      mutated_code = "code",
      env = new.env(),
      reporter = testthat::SilentReporter$new()
    )

    # Assert
    expect_equal(sum(as.data.frame(result)$passed), 2)
  })
})

describe("FileTestStrategy", {
  it("runs only tests matching the source file name", {
    # Arrange
    temp_dir <- withr::local_tempdir()
    test_dir <- file.path(temp_dir, "tests/testthat")
    dir.create(test_dir, recursive = TRUE)

    test1 <- 'test_that("test1", { expect_true(TRUE) })'
    test2 <- 'test_that("test2", { expect_true(TRUE) })'
    writeLines(test1, file.path(test_dir, "test-file1.R"))
    writeLines(test2, file.path(test_dir, "test-file2.R"))
    strategy <- FileTestStrategy$new(load_package = "none")

    # Act
    result <- strategy$execute(
      path = test_dir,
      mutated_file = "file1.R",
      mutated_code = "code",
      env = new.env(),
      reporter = testthat::SilentReporter$new()
    )

    # Assert
    expect_equal(sum(as.data.frame(result)$passed), 1)
  })

  it("doesn't run test files if source file name doesn't match", {
    # Arrange
    temp_dir <- withr::local_tempdir()
    test_dir <- file.path(temp_dir, "tests/testthat")
    dir.create(test_dir, recursive = TRUE)

    test1 <- 'test_that("test1", { expect_true(TRUE) })'
    test2 <- 'test_that("test2", { expect_true(TRUE) })'
    writeLines(test1, file.path(test_dir, "test-file1.R"))
    writeLines(test2, file.path(test_dir, "test-file2.R"))
    strategy <- FileTestStrategy$new(load_package = "none")

    # Act
    result <- strategy$execute(
      path = test_dir,
      mutated_file = "file3.R",
      mutated_code = "code",
      env = new.env(),
      reporter = testthat::SilentReporter$new()
    )

    # Assert
    expect_equal(length(result), 0)
  })
})


test_that("default_test_strategy returns a FileTestStrategy", {
  strategy <- default_test_strategy()
  expect_s3_class(strategy, "R6")
  expect_true(inherits(strategy, "FileTestStrategy"))
  expect_true(inherits(strategy, "TestStrategy"))
})
