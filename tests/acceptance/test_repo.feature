Feature: Test real R package

  This specification ensures that {muttest} works for the few selected, popular packages.

  Scenario: Testing dplyr
    Given I clone a repository from "https://github.com/tidyverse/dplyr/archive/main.zip"
    When I run mutation tests with
      """
      plan <- plan(
        mutators = list(
          operator("+", "-")
        )
      )
      muttest(
        path = "tests/testthat",
        plan = plan[1:10, ],
        test_strategy = FileTestStrategy$new()
      )
      """
    Then I get a mutation score

  Scenario: Testing ggplot2
    Given I clone a repository from "https://github.com/tidyverse/ggplot2/archive/main.zip"
    When I run mutation tests with
      """
      plan <- plan(
        mutators = list(
          operator("+", "-")
        )
      )
      muttest(
        path = "tests/testthat",
        plan = plan[1:10, ],
        test_strategy = FileTestStrategy$new()
      )
      """
    Then I get a mutation score

  Scenario: Testing shiny
    Given I clone a repository from "https://github.com/rstudio/shiny/archive/main.zip"
    When I run mutation tests with
      """
      plan <- plan(
        mutators = list(
          operator("==", "!=")
        )
      )
      muttest(
        path = "tests/testthat",
        plan = plan[1, ]
      )
      """
    Then I get a mutation score
