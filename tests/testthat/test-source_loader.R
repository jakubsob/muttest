describe("null_source_loader", {
  it("should return a function that does nothing", {
    # Arrange
    loader <- null_loader()

    # Act
    result <- loader("some_dir")

    # Assert
    expect_type(result, "environment")
    expect_equal(ls(result), character(0))
  })
})

describe("dir_source_loader", {
  it("should load R files from the specified directory", {
    # Arrange
    temp_dir <- withr::local_tempdir()
    dir.create(file.path(temp_dir, "R"))
    writeLines("x <- 1", file.path(temp_dir, "R", "test.R"))
    loader <- dir_loader()

    # Act
    result <- loader(temp_dir)

    # Assert
    expect_type(result, "environment")
    expect_equal(result$x, 1)
  })

  it("should not load non-R files", {
    # Arrange
    temp_dir <- withr::local_tempdir()
    dir.create(file.path(temp_dir, "R"))
    writeLines("x <- 1", file.path(temp_dir, "R", "test.txt"))
    loader <- dir_loader()

    # Act
    result <- loader(temp_dir)

    # Assert
    expect_type(result, "environment")
    expect_null(result$x)
  })
})
