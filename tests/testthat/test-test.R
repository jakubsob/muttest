test_that("which_bigger", {
  withr::with_dir(system.file("examples/operators/", package = "muttest"), {
    expect_equal(test("tests/testthat"), 0.5)
  })
})

test_that("box", {
  testthat::skip_if(testthat::is_checking())
  withr::with_dir(system.file("examples/box/", package = "muttest"), {
    expect_equal(test("tests/testthat"), 1)
  })
})
