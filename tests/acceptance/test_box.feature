Feature: Test klmr::box project

  This is a specification of how mutation testing of a project using klmr::box works.

  Scenario: Run plan with one source file and one mutation
    Given I have a "src/calculate.R" file with
      """
      #' @export
      calculate <- function(x) {
        x + 1
      }
      """
    And I have a "tests/test-calculate.R" file with
      """
      box::use(../src/calculate[calculate])

      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    When I run mutation tests with
      """
      test(
        plan = test_plan(
          mutators = list(
            operator("+", "-")
          ),
          source_files = c("src/calculate.R")
        ),
        path = "tests",
        test_strategy = FileTestStrategy$new(load_package = "none")
      )
      """
    Then the mutation score should be 1.0

  Scenario: Function imported to a test file with the same name
    Given I have a "src/calculate.R" file with
      """
      #' @export
      calculate <- function(x) {
        x + 1
      }
      """
    Given I have a "src/calculate2.R" file with
      """
      #' @export
      calculate <- function(x) {
        x * 1
      }
      """
    And I have a "tests/test-calculate.R" file with
      """
      box::use(../src/calculate[calculate])

      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    And I have a "tests/test-calculate2.R" file with
      """
      box::use(../src/calculate2[calculate])

      test_that("calculate multiplies by 1", {
        expect_equal(calculate(1), 1)
      })
      """
    When I run mutation tests with
      """
      plan <- test_plan(
        mutators = list(
          operator("+", "-"),
          operator("*", "/")
        ),
        source_files = c("src/calculate.R", "src/calculate2.R")
      )
      test(
        plan = plan,
        path = "tests",
        test_strategy = FileTestStrategy$new(load_package = "none")
      )
      """
    Then the mutation score should be 0.5
