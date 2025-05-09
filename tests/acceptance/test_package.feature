Feature: Test package

  This is a specification of how mutation testing of a package works.
  It is the default behavior, requiring the least amount of configuration to run tests.

  Scenario: Run plan with one source file and one mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    Given I have a "R/calculate.R" file with
      """
      calculate <- function(x) {
        x + 1
      }
      """
    And I have a "tests/testthat/test-calculate.R" file with
      """
      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    When I run mutation tests with
      """
      test(
        path = "tests/testthat",
        plan = test_plan(
          mutators = list(
            operator("+", "-")
          )
        )
      )
      """
    Then the mutation score should be 1.0

  Scenario: Run plan with one source file and multiple mutations
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    Given I have a "R/calculate.R" file with
      """
      calculate <- function(x) {
        x + 1 / 2
      }
      """
    And I have a "tests/testthat/test-calculate.R" file with
      """
      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    When I run mutation tests with
      """
      test(
        path = "tests/testthat",
        plan = test_plan(
          mutators = list(
            operator("+", "-"),
            operator("/", "*")
          )
        )
      )
      """
    Then the mutation score should be 1.0

  Scenario: Run plan with multiple source files and multiple mutations
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    Given I have a "R/calculate.R" file with
      """
      calculate <- function(x) {
        x + 1
      }
      """
    And I have a "R/another_file.R" file with
      """
      another_function <- function(x) {
        x * 2
      }
      """
    And I have a "tests/testthat/test-calculate.R" file with
      """
      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    And I have a "tests/testthat/test-another_file.R" file with
      """
      test_that("another_function multiplies by 2", {
        expect_equal(another_function(2), 4)
      })
      """
    When I run mutation tests with
      """
      test(
        path = "tests/testthat",
        plan = test_plan(
          mutators = list(
            operator("+", "-"),
            operator("/", "*")
          )
        )
      )
      """
    Then the mutation score should be 1.0

  Scenario: Run plan with no relevant mutations

    If no mutations can be applied to the code, the mutation score is NA.

    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    Given I have a "R/calculate.R" file with
      """
      calculate <- function(x) {
        x + 1
      }
      """
    And I have a "tests/testthat/test-calculate.R" file with
      """
      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    When I run mutation tests with
      """
      test(
        path = "tests/testthat",
        plan = test_plan(
          mutators = list(
            operator("*", "/")
          )
        )
      )
      """
    Then the mutation score should be NA

  Scenario: Test runs with errors don't count in the mutation score

    Mutating "+" to "-" triggers an error in the function and the assertion doesn't pass.
    It's not a failute of a test, but a failure of the function.
    This mutation is not counted in the mutation score.
    Only change from "+" to "*" is counted.
    There are 2 mutations, 1 error, 1 killed, score is 50%.

    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    Given I have a "R/calculate.R" file with
      """
      calculate <- function(x) {
        score <- x + 1
        if (score == 0) {
          stop("Score is zero")
        }
        score
      }
      """
    And I have a "tests/testthat/test-calculate.R" file with
      """
      test_that("calculate adds 1", {
        expect_equal(calculate(1), 2)
      })
      """
    When I run mutation tests with
      """
      test(
        path = "tests/testthat",
        plan = test_plan(
          mutators = list(
            operator("+", "-"),
            operator("+", "*")
          )
        )
      )
      """
    Then the mutation score should be 0.5
