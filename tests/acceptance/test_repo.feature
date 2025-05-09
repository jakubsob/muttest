Feature: Mutation testing of selected R packages

  This specification ensures that {muttest} works for the few selected, popular packages.

  Scenario: Testing dplyr
    Given I clone a repository from "https://github.com/tidyverse/dplyr/archive/main.zip"
    When I run mutation tests with
      """
      plan <- test_plan(
        mutators = list(
          operator("+", "-")
        )
      )
      test(
        path = "tests/testthat",
        plan = plan[1, ]
      )
      """
    Then I get a mutation score

  Scenario: Testing ggplot2
    Given I clone a repository from "https://github.com/tidyverse/ggplot2/archive/main.zip"
    When I run mutation tests with
      """
      plan <- test_plan(
        mutators = list(
          operator("+", "-")
        )
      )
      test(
        path = "tests/testthat",
        plan = plan[1, ]
      )
      """
    Then I get a mutation score

  Scenario: Testing shiny
    Given I clone a repository from "https://github.com/rstudio/shiny/archive/main.zip"
    When I run mutation tests with
      """
      plan <- test_plan(
        mutators = list(
          operator("==", "!="),
          operator("!=", "==")
        )
      )
      test(
        path = "tests/testthat",
        plan = plan[1, ]
      )
      """
    Then I get a mutation score
