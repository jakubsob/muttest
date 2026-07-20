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
      mutator = I(list(negate_condition("while"))),
      # unused by the base reporter; present only to satisfy the plan contract
      mutation = I(list(NULL))
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
    # Errors count as detected (killed), so an all-errors run scores 1.
    expect_equal(as.numeric(result), 1)
  })
})


.tests <- list(
  list(
    title = "session with ProgressReporter prints results",
    reporter = function() ProgressMutationReporter$new(),
    mutators = list(operator(">", "<"), operator(">", ">=")),
    test_strategy = function() default_test_strategy()
  ),
  list(
    title = "no-coverage mutants are reported and excluded from the score",
    reporter = function() {
      ProgressMutationReporter$new(survived_detail = "none")
    },
    # discount.R has no matching test file -> no coverage; shipping.R is killed.
    mutators = list(operator(">", "<"), operator("-", "+")),
    test_strategy = function() {
      FileTestStrategy$new(load_helpers = FALSE, load_package = "none")
    }
  )
)

for (t in .tests) {
  local({
    test_that(t$title, {
      .with_example_dir("shipping/", {
        p <- muttest_plan(t$mutators, fs::dir_ls("R"))
        result <- .muttest(
          p,
          reporter = t$reporter(),
          test_strategy = t$test_strategy()
        )
        expect_snapshot({
          print(p)
          print(result)
        })
      })
    })
  })
}
