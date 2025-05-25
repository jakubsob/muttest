#' @title Reporter for Mutation Testing
#'
#' @description
#' The job of a mutation reporter is to aggregate and display the results of mutation tests.
#' It tracks each mutation attempt, reporting on whether the tests killed the mutation or the mutation survived.
#'
#' @field test_reporter Reporter to use for the testthat::test_dir function
#' @field out Output destination for reporter messages
#' @field width Width of the console in characters
#' @field unicode Whether Unicode output is supported
#' @field crayon Whether colored output is supported
#' @field rstudio Whether running in RStudio
#' @field hyperlinks Whether terminal hyperlinks are supported
#' @field current_file Path of the file currently being mutated
#' @field current_mutator Mutator currently being applied
#' @field plan Complete mutation plan for the test run
#' @field results List of mutation test results, indexed by file path
#' @field current_score Current score of the mutation tests
#'
#' @md
#' @export
#' @importFrom rlang `%||%`
#' @family MutationReporter
MutationReporter <- R6::R6Class(
  classname = "MutationReporter",
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

    # Current state
    current_file = NULL,
    current_mutator = NULL,

    plan = NULL,

    # Track mutations by file
    results = NULL,
    current_score = NA_real_,

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
    start_reporter = function(plan = NULL) {
      self$plan <- plan
      self$results <- list()
      self$current_score <- NA_real_
    },

    #' @description Start testing a file
    #' @param filename Path to the file being mutated
    start_file = function(filename) {
      self$current_file <- filename
      self$results[[filename]] <- self$results[[filename]] %||%
        list(
          total = 0,
          killed = 0,
          survived = 0,
          errors = 0
        )
    },

    #' @description Start testing with a specific mutator
    #' @param mutator The mutator being applied
    start_mutator = function(mutator) {
      self$current_mutator <- mutator
    },

    #' @description Add a mutation test result
    #' @param plan Current testing plan. See `test_plan()`.
    #' @param killed Whether the mutation was killed by tests
    #' @param survived Number of survived mutations
    #' @param errors Number of errors encountered
    #' @md
    add_result = function(
      plan,
      killed,
      survived,
      errors
    ) {
      filename <- plan$filename
      self$results[[filename]]$total <- self$results[[filename]]$total + 1
      self$results[[filename]]$killed <- self$results[[filename]]$killed +
        killed
      self$results[[filename]]$survived <- self$results[[filename]]$survived +
        survived
      self$results[[filename]]$errors <- self$results[[filename]]$errors +
        errors
      killed_counts <- purrr::map(self$results, "killed")
      total_counts <- purrr::map(self$results, "total")
      self$current_score <- sum(as.numeric(killed_counts)) /
        sum(as.numeric(total_counts))
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
    },

    #' @description Get the current score
    get_score = function() {
      self$current_score
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

#' Create a default reporter
#'
#' @param ... Arguments passed to the `?MutationProgressReporter` constructor.
#' @md
#' @export
#' @family MutationReporter
default_reporter <- function(...) {
  MutationProgressReporter$new(...)
}
