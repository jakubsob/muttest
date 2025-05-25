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
