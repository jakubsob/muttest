describe("CopyStrategy", {
  it("throws error when calling execute", {
    # Arrange
    strategy <- CopyStrategy$new()

    # Act, Assert
    expect_error(strategy$execute("original_dir"), "Not implemented")
  })
})

describe("PackageCopyStrategy", {
  it("copies project files excluding hidden and temp directories", {
    # Arrange
    temp_dir <- withr::local_tempdir()
    original_dir <- file.path(temp_dir, "original")
    dir.create(original_dir)
    dir.create(file.path(original_dir, ".hidden"))
    dir.create(file.path(original_dir, "tmp"))
    dir.create(file.path(original_dir, "temp"))
    dir.create(file.path(original_dir, "inst"))
    dir.create(file.path(original_dir, "src"))

    strategy <- PackageCopyStrategy$new()

    # Act
    copied_dir <- strategy$execute(original_dir, data.frame())

    # Assert
    expect_true(dir.exists(copied_dir))
    expect_false(dir.exists(file.path(copied_dir, ".hidden")))
    expect_false(dir.exists(file.path(copied_dir, "tmp")))
    expect_false(dir.exists(file.path(copied_dir, "temp")))
    expect_true(dir.exists(file.path(copied_dir, "src")))
    expect_true(dir.exists(file.path(copied_dir, "inst")))
  })
})

describe("default_copy_strategy", {
  it("returns a CopyStrategy object", {
    # Act
    strategy <- default_copy_strategy()

    # Assert
    expect_s3_class(strategy, "CopyStrategy")
  })
})
