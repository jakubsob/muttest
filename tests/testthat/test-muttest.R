.with_example_dir <- function(path, code) {
  withr::with_dir(
    system.file("examples", path, package = "muttest"),
    code
  )
}

test_ <- function(...) {
  purrr::quietly(muttest)(...)$result
}

test_that("operators", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- plan(mutators, fs::dir_ls("R"))
    expect_equal(
      test_(plan),
      0.5
    )
  })
})

test_that("test runner errors are recorded as errors, not propagated", {
  error_strategy <- R6::R6Class(
    inherit = TestStrategy,
    public = list(
      execute = function(path, plan, reporter) stop("test runner crashed")
    )
  )$new()

  .with_example_dir("operators/", {
    p <- plan(list(operator("+", "-")), fs::dir_ls("R"))
    score <- test_(p, test_strategy = error_strategy)
    expect_equal(score, 0)
  })
})
