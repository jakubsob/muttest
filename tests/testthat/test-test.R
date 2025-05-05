.with_example_dir <- function(path, code) {
  withr::with_dir(
    system.file("examples", path, package = "muttest"),
    code
  )
}

test_that("operators", {
  .with_example_dir("operators/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    expect_equal(purrr::quietly(test)("tests/testthat", mutators)$result, 0.5)
  })
})

test_that("multiple_files", {
  .with_example_dir("multiple_files/", {
    mutators <- list(operator("+", "-"), operator("*", "/"))
    expect_equal(
      purrr::quietly(test)("tests/testthat", mutators = mutators)$result,
      0.333,
      tolerance = 0.01
    )
  })
})

test_that("no_mutations", {
  .with_example_dir("no_mutations/", {
    mutators <- list(operator("/", "*"))
    expect_equal(
      purrr::quietly(test)("tests/testthat", mutators = mutators)$result,
      1
    )
  })
})

test_that("box", {
  testthat::skip_if(testthat::is_checking())
  .with_example_dir("box/", {
    mutators <- list(operator("+", "-"))
    expect_equal(
      purrr::quietly(test)("tests/testthat", mutators = mutators)$result,
      1
    )
  })
})
