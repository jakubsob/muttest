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

describe("PackageCopyStrategy(symlink = TRUE)", {
  # A project with one file in the mutated dir (R/) and one in another dir (inst/).
  make_project <- function(mutated_file = "before", other_file = "before") {
    original_dir <- withr::local_tempdir(.local_envir = parent.frame())
    dir.create(file.path(original_dir, "R"))
    dir.create(file.path(original_dir, "inst"))
    writeLines(mutated_file, file.path(original_dir, "R", "source.R"))
    writeLines(other_file, file.path(original_dir, "inst", "fixture.txt"))
    original_dir
  }

  it("writing the mutated file (R/source.R) stays isolated from the original", {
    # Arrange
    original_dir <- make_project(mutated_file = "before")
    plan <- data.frame(filename = "R/source.R", stringsAsFactors = FALSE)
    copied_dir <- PackageCopyStrategy$new(symlink = TRUE)$execute(original_dir, plan)
    withr::defer(fs::dir_delete(copied_dir))

    # Act: overwrite the mutated file in the copy
    writeLines("after", file.path(copied_dir, "R", "source.R"))

    # Assert: R/ is a real copy, so the original R/source.R is untouched
    expect_false(fs::is_link(file.path(copied_dir, "R")))
    expect_equal(readLines(file.path(original_dir, "R", "source.R")), "before")
  })

  it("writing into a symlinked dir (inst/fixture.txt) leaks to the original (known limitation)", {
    # Arrange
    original_dir <- make_project(other_file = "before")
    plan <- data.frame(filename = "R/source.R", stringsAsFactors = FALSE)
    copied_dir <- PackageCopyStrategy$new(symlink = TRUE)$execute(original_dir, plan)
    withr::defer(fs::dir_delete(copied_dir))

    # Act: write through the (symlinked) inst dir
    writeLines("after", file.path(copied_dir, "inst", "fixture.txt"))

    # Assert: inst/ is a symlink, so the write reached the original inst/fixture.txt
    expect_true(fs::is_link(file.path(copied_dir, "inst")))
    expect_equal(readLines(file.path(original_dir, "inst", "fixture.txt")), "after")
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
