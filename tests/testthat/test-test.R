test_that("operators", {
  withr::with_dir(system.file("examples/operators/", package = "muttest"), {
    expect_equal(test("tests/testthat"), 0.5)
  })
})

test_that("multiple_files", {
  withr::with_dir(system.file("examples/multiple_files/", package = "muttest"), {
    expect_equal(test("tests/testthat"), 0.333, tolerance = 0.01)
  })
})
test_that("no_mutations", {
  withr::with_dir(system.file("examples/no_mutations/", package = "muttest"), {
    expect_equal(test("tests/testthat", mutators = list(operator("/", "*"))), 1)
  })
})

test_that("box", {
  testthat::skip_if(testthat::is_checking())
  withr::with_dir(system.file("examples/box/", package = "muttest"), {
    expect_equal(test("tests/testthat", mutators = list(operator("+", "-"))), 1)
  })
})
