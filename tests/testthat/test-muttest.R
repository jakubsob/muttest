test_that("plan returns muttest_plan when no mutations apply", {
  .with_example_dir("operators/", {
    p <- plan(list(operator("*", "/")), fs::dir_ls("R"))
    expect_s3_class(p, "muttest_plan")
  })
})

test_that("operators", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    plan <- plan(mutators, fs::dir_ls("R"))
    expect_equal(
      .muttest(plan),
      0.5
    )
  })
})

test_that("timeout on infinite loop is recorded as error", {
  skip_on_cran()
  .with_example_dir("operators/", {
    original <- readLines("R/calculate.R")
    mutated <- c("calculate <- function(x, y) {", "  while (TRUE) {}", "}")
    p <- muttest_plan(data.frame(
      filename = "R/calculate.R",
      original_code = I(list(original)),
      mutated_code = I(list(mutated)),
      mutator = I(list(negate_condition("while")))
    ))
    reporter <- MutationReporter$new()
    capture.output(suppressMessages(suppressWarnings(muttest(p, reporter = reporter, timeout = 400))), type = "output")
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
    score <- .muttest(p, test_strategy = error_strategy)
    expect_equal(score, 0)
  })
})

test_that("muttest_plan can first n mutants", {
  p <- muttest_plan(data.frame(
    filename = c("R/calculate.R", "R/calculate.R"),
    original_code = I(list(
      c("calculate <- function(x, y) {", "  x + y", "}"),
      c("calculate <- function(x, y) {", "  x + y", "}")
    )),
    mutated_code = I(list(
      c("calculate <- function(x, y) {", "  x - y", "}"),
      c("calculate <- function(x, y) {", "  x - y", "}")
    )),
    mutator = I(list(operator("+", "-"), operator("+", "-")))
  ))
  .expect_snapshot(print(p, nrows = 1))
})

test_that("muttest_plan prints all mutants", {
  p <- muttest_plan(data.frame(
    filename = c("R/calculate.R", "R/calculate.R"),
    original_code = I(list(
      c("calculate <- function(x, y) {", "  x + y", "}"),
      c("calculate <- function(x, y) {", "  x + y", "}")
    )),
    mutated_code = I(list(
      c("calculate <- function(x, y) {", "  x - y", "}"),
      c("calculate <- function(x, y) {", "  x - y", "}")
    )),
    mutator = I(list(operator("+", "-"), operator("+", "-")))
  ))
  .expect_snapshot(print(p, nrows = 2))
})
