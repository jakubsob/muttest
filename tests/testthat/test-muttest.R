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

test_that("timeout on infinite loop is recorded as error", {
  skip_on_cran()
  .with_example_dir("operators/", {
    original <- readLines("R/calculate.R")
    mutated <- c("calculate <- function(x, y) {", "  while (TRUE) {}", "}")
    p <- tibble::tibble(
      filename = "R/calculate.R",
      original_code = list(original),
      mutated_code = list(mutated),
      mutator = list(negate_condition("while"))
    )
    reporter <- MutationReporter$new()
    purrr::quietly(muttest)(p, reporter = reporter, timeout = 400)
    expect_equal(reporter$results[["R/calculate.R"]]$errors, 1)
    expect_equal(reporter$error_messages[[1]], "Timed out")
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
