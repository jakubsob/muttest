test_that("plan returns .muttest_plan when no mutations apply", {
  .with_example_dir("operators/", {
    p <- muttest_plan(list(operator("*", "/")), fs::dir_ls("R"))
    expect_s3_class(p, "muttest_plan")
  })
})

test_that("timeout on infinite loop is recorded as error", {
  skip_on_cran()
  .with_example_dir("operators/", {
    original <- readLines("R/calculate.R")
    mutated <- c("calculate <- function(x, y) {", "  while (TRUE) {}", "}")
    p <- .muttest_plan(data.frame(
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
    p <- muttest_plan(list(operator("+", "-")), fs::dir_ls("R"))
    result <- .muttest(p, test_strategy = error_strategy)
    expect_equal(as.numeric(result), 0)
  })
})


.tests <- list(
  list(
    title = "session with ProgressReporter prints results",
    reporter = function() ProgressMutationReporter$new()
  )
)

for (t in .tests) {
  local({
    test_that(t$title, {
      .with_example_dir("shipping/", {
        mutators <- list(operator(">", "<"), operator(">", ">="))
        p <- muttest_plan(mutators, fs::dir_ls("R"))
        result <- .muttest(p, reporter = t$reporter())
        expect_snapshot({
          print(p)
          print(result)
        })
      })
    })
  })
}
