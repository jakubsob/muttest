Feature: Mutators

  Acceptance tests for every mutator family added beyond the basic operator() mutator.
  Each scenario verifies that well-written tests can kill the relevant mutant (score = 1.0).

  Scenario: boolean_literal kills TRUE → FALSE mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/guard.R" file with
      """
      is_valid <- function(x) {
        if (is.null(x)) return(FALSE)
        TRUE
      }
      """
    And I have a "tests/testthat/test-guard.R" file with
      """
      test_that("returns FALSE for NULL", { expect_false(is_valid(NULL)) })
      test_that("returns TRUE for non-NULL", { expect_true(is_valid(1)) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(boolean_literal("TRUE", "FALSE")))
      )
      """
    Then the mutation score should be 1.0

  Scenario: boolean_literal kills FALSE → TRUE mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/guard.R" file with
      """
      is_valid <- function(x) {
        if (is.null(x)) return(FALSE)
        TRUE
      }
      """
    And I have a "tests/testthat/test-guard.R" file with
      """
      test_that("returns FALSE for NULL", { expect_false(is_valid(NULL)) })
      test_that("returns TRUE for non-NULL", { expect_true(is_valid(1)) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(boolean_literal("FALSE", "TRUE")))
      )
      """
    Then the mutation score should be 1.0

  Scenario: call_name kills any → all mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/check.R" file with
      """
      has_positive <- function(x) any(x > 0)
      """
    And I have a "tests/testthat/test-check.R" file with
      """
      test_that("TRUE when at least one positive", { expect_true(has_positive(c(-1, 1))) })
      test_that("FALSE when all non-positive",     { expect_false(has_positive(c(-2, -1))) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(call_name("any", "all")))
      )
      """
    Then the mutation score should be 1.0

  Scenario: call_name kills min → max mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/stats.R" file with
      """
      smallest <- function(x) min(x)
      """
    And I have a "tests/testthat/test-stats.R" file with
      """
      test_that("returns minimum", { expect_equal(smallest(c(3, 1, 2)), 1) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(call_name("min", "max")))
      )
      """
    Then the mutation score should be 1.0

  Scenario: string_empty kills non-empty string → "" mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/label.R" file with
      """
      default_label <- function(label) {
        if (nchar(label) == 0) return("unknown")
        label
      }
      """
    And I have a "tests/testthat/test-label.R" file with
      """
      test_that("empty input gives unknown", { expect_equal(default_label(""), "unknown") })
      test_that("non-empty input returns itself", { expect_equal(default_label("foo"), "foo") })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(string_empty()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: string_fill kills "" → "mutant" mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/label.R" file with
      """
      default_label <- function(label) {
        if (label == "") return("unknown")
        label
      }
      """
    And I have a "tests/testthat/test-label.R" file with
      """
      test_that("empty string returns unknown", { expect_equal(default_label(""), "unknown") })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(string_fill()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: numeric_increment kills n → n+1 mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/math.R" file with
      """
      add_one <- function(x) x + 1
      """
    And I have a "tests/testthat/test-math.R" file with
      """
      test_that("adds exactly one", { expect_equal(add_one(5), 6) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(numeric_increment()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: numeric_decrement kills n → n-1 mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/math.R" file with
      """
      add_one <- function(x) x + 1
      """
    And I have a "tests/testthat/test-math.R" file with
      """
      test_that("adds exactly one", { expect_equal(add_one(5), 6) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(numeric_decrement()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: negate_condition kills negated if condition
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/classify.R" file with
      """
      sign_of <- function(x) {
        if (x > 0) "positive" else "non-positive"
      }
      """
    And I have a "tests/testthat/test-classify.R" file with
      """
      test_that("positive number", { expect_equal(sign_of(1), "positive") })
      test_that("negative number", { expect_equal(sign_of(-1), "non-positive") })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(negate_condition()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: remove_condition_negation kills ! removal from if condition
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/loop.R" file with
      """
      count_until <- function(x) {
        i <- 0
        while (!is.null(x) && i < 10) i <- i + 1
        i
      }
      """
    And I have a "tests/testthat/test-loop.R" file with
      """
      test_that("counts to 10 for non-null", { expect_equal(count_until(1), 10) })
      test_that("counts zero for null",       { expect_equal(count_until(NULL), 0) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(remove_condition_negation()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: remove_negation kills ! removal mutation
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/filter.R" file with
      """
      is_present <- function(x) !is.null(x)
      """
    And I have a "tests/testthat/test-filter.R" file with
      """
      test_that("NULL is absent",     { expect_false(is_present(NULL)) })
      test_that("non-NULL is present", { expect_true(is_present(1)) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = list(remove_negation()))
      )
      """
    Then the mutation score should be 1.0

  Scenario: arithmetic_operators preset kills mutation in arithmetic code
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/math.R" file with
      """
      scale <- function(x, factor) x * factor
      """
    And I have a "tests/testthat/test-math.R" file with
      """
      test_that("multiplies correctly", { expect_equal(scale(3, 4), 12) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = arithmetic_operators())
      )
      """
    Then the mutation score should be 1.0

  Scenario: comparison_operators preset kills mutation in comparison code
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/check.R" file with
      """
      is_adult <- function(age) age >= 18
      """
    And I have a "tests/testthat/test-check.R" file with
      """
      test_that("18 is adult",    { expect_true(is_adult(18)) })
      test_that("17 is not adult", { expect_false(is_adult(17)) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = comparison_operators())
      )
      """
    Then the mutation score should be 1.0

  Scenario: logical_operators preset kills mutation in logical code
    Given I have a "DESCRIPTION" file with
      """
      Package: example
      Version: 0.1.0
      """
    And I have a "R/check.R" file with
      """
      in_range <- function(x, lo, hi) x >= lo && x <= hi
      """
    And I have a "tests/testthat/test-check.R" file with
      """
      test_that("in range",     { expect_true(in_range(5, 1, 10)) })
      test_that("below range",  { expect_false(in_range(0, 1, 10)) })
      test_that("above range",  { expect_false(in_range(11, 1, 10)) })
      """
    When I run mutation tests with
      """
      muttest(
        path = "tests/testthat",
        plan = muttest_plan(mutators = logical_operators())
      )
      """
    Then the mutation score should be 1.0
