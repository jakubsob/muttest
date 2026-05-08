.with_example_dir <- function(path, code) {
  withr::with_dir(
    system.file("examples", path, package = "muttest"),
    code
  )
}

test_ <- function(...) {
  purrr::quietly(muttest)(...)$result
}

test_that("workers = 2 produces the same score as workers = 1", {
  skip_on_cran()
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    p <- plan(mutators, fs::dir_ls("R"))
    expect_equal(
      test_(p, workers = 2),
      test_(p, workers = 1)
    )
  })
})

test_that("workers must be a positive integer", {
  .with_example_dir("operators/", {
    p <- plan(list(operator("+", "-")), fs::dir_ls("R"))
    expect_error(muttest(p, workers = 0))
    expect_error(muttest(p, workers = -1))
    expect_error(muttest(p, workers = 1.5))
  })
})
