#' Reporter for Mutation Testing
#'
#' The job of a mutation reporter is to aggregate and display the results
#' of mutation tests. It tracks each mutation attempt, reporting on whether
#' the tests killed the mutation or the mutation survived.
#'
#' @field test_reporter Reporter to use for the testthat::test_dir function
#' @field out Output destination for reporter messages
#' @field width Width of the console in characters
#' @field unicode Whether Unicode output is supported
#' @field crayon Whether colored output is supported
#' @field rstudio Whether running in RStudio
#' @field hyperlinks Whether terminal hyperlinks are supported
#' @field total_mutations Total number of mutation tests executed
#' @field killed_mutations Number of mutations killed by tests
#' @field survived_mutations Number of mutations that survived tests
#' @field current_file Path of the file currently being mutated
#' @field current_mutator Mutator currently being applied
#' @field plan Complete mutation plan for the test run
#' @field temp_dir Path to the temporary directory for testing
#'
#' @export
#' @family reporters
MutationReporter <- R6::R6Class("MutationReporter",
  public = list(
    # Define test reporter that will be used for running tests
    test_reporter = NULL,

    # Output destination
    out = NULL,

    # Display settings
    width = 80,
    unicode = TRUE,
    crayon = TRUE,
    rstudio = TRUE,
    hyperlinks = TRUE,

    # Counters for tracking mutation results
    total_mutations = 0,
    killed_mutations = 0,
    survived_mutations = 0,

    # Current state
    current_file = NULL,
    current_mutator = NULL,

    # Mutation plan and temp directory
    plan = NULL,
    temp_dir = NULL,

    #' @description Initialize a new reporter
    #' @param test_reporter Reporter to use for the testthat::test_dir function
    #' @param file Output destination (default: stdout)
    initialize = function(test_reporter = "silent", file = stdout()) {
      self$test_reporter <- test_reporter
      self$out <- file

      # Capture display settings
      self$width <- cli::console_width()
      self$unicode <- cli::is_utf8_output()
      self$crayon <- cli::num_ansi_colors() > 1
      self$rstudio <- Sys.getenv("RSTUDIO") == "1"
      self$hyperlinks <- cli::ansi_hyperlink_types()[["run"]]
    },

    #' @description Start reporter
    #' @param plan The complete mutation plan
    #' @param temp_dir Path to the temporary directory for testing
    start_reporter = function(plan = NULL, temp_dir = NULL) {
      self$total_mutations <- 0
      self$killed_mutations <- 0
      self$survived_mutations <- 0
      self$plan <- plan
      self$temp_dir <- temp_dir
    },

    #' @description Start testing a file
    #' @param file_path Path to the file being mutated
    start_file = function(file_path) {
      self$current_file <- file_path
    },

    #' @description Start testing with a specific mutator
    #' @param mutator The mutator being applied
    start_mutator = function(mutator) {
      self$current_mutator <- mutator
    },

    #' @description Add a mutation test result
    #' @param file_path Path to the file that was mutated
    #' @param mutator The mutator that was applied
    #' @param test_results Results from the test execution
    #' @param killed Whether the mutation was killed by tests
    add_result = function(file_path, mutator, test_results, killed) {
      self$total_mutations <- self$total_mutations + 1
      if (killed) {
        self$killed_mutations <- self$killed_mutations + 1
      } else {
        self$survived_mutations <- self$survived_mutations + 1
      }
    },

    #' @description End testing with current mutator
    end_mutator = function() {
      self$current_mutator <- NULL
    },

    #' @description End testing current file
    end_file = function() {
      self$current_file <- NULL
    },

    #' @description End reporter and show summary
    end_reporter = function() {
      score <- self$killed_mutations / self$total_mutations
      self$cat_line()
      cli::cli_rule(cli::style_bold("Mutation Testing Results"))
      self$cat_line(
        "[ ",
        cli::col_green("KILLED "),
        self$killed_mutations,
        " | ",
        cli::col_red("SURVIVED "),
        self$survived_mutations,
        " | ",
        paste0("TOTAL ", self$total_mutations),
        " | ",
        cli::col_green("SCORE: "),
        sprintf("%.1f%%", score * 100),
        " ]"
      )

      self$cat_line()
    },

    #' @description Print a message to the output
    #' @param ... Message to print
    # nocov start
    cat_tight = function(...) {
      cat(..., sep = "", file = self$out, append = TRUE)
    },
    # nocov end

    #' @description Print a message to the output
    #' @param ... Message to print
    cat_line = function(...) {
      cli::cat_line(..., file = self$out)
    },

    #' @description Print a message to the output with a rule
    #' @param ... Message to print
    rule = function(...) {
      cli::cat_rule(..., file = self$out)
    }
  )
)

#' Get a default reporter
#'
#' @param ... Additional arguments passed to reporter constructor
#' @export
default_reporter <- function(...) {
  MutationProgressReporter$new(...)
}
