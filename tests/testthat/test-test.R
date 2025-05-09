.with_example_dir <- function(path, code) {
  withr::with_dir(
    system.file("examples", path, package = "muttest"),
    code
  )
}

test_ <- function(...) {
  purrr::quietly(test)(...)$result
}

test_that("operators", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- test_plan(fs::dir_ls("R"), mutators)
    expect_equal(
      test_(path = "tests/testthat", plan = plan),
      0.5
    )
  })
})

test_that("multiple_files", {
  .with_example_dir("multiple_files/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- test_plan(fs::dir_ls("R"), mutators)
    expect_equal(
      test_(path = "tests/testthat", plan = plan),
      0.333,
      tolerance = 0.01
    )
  })
})

test_that("no_mutations", {
  .with_example_dir("no_mutations/", {
    mutators <- list(operator("/", "*"))
    plan <- test_plan(fs::dir_ls("R"), mutators)
    expect_equal(
      test_(path = "tests/testthat", plan = plan),
      NA_real_
    )
  })
})
