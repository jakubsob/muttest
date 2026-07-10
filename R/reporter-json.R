#' @title JSON Reporter for Mutation Testing
#'
#' @description
#' A quiet reporter that writes a machine-readable JSON artifact. It prints
#' nothing while running; on completion it writes one JSON file describing every
#' mutant, keyed by source file. The JSON is the data contract consumed by
#' dashboards, CI job summaries, and the HTML report (see
#' [report()]).
#'
#' Combine it with [ProgressMutationReporter] via [MultiReporter] to get a live
#' console display and a JSON file from the same run.
#'
#' @field path Path of the JSON file to write.
#' @field mutants_by_file Per-file lists of mutant records.
#' @field sources Per-file original source lines.
#'
#' @md
#' @export
#' @family MutationReporter
JSONMutationReporter <- R6::R6Class(
  classname = "JSONMutationReporter",
  inherit = MutationReporter,
  public = list(
    path = NULL,
    mutants_by_file = list(),
    sources = list(),

    #' @description Initialize a new JSON reporter
    #' @param path Path of the JSON file to write (default: `"muttest.json"`).
    #' @param ... Passed to the [MutationReporter] constructor.
    initialize = function(path = "muttest.json", ...) {
      super$initialize(...)
      self$path <- path
    },

    #' @description Start reporter
    #' @param plan The complete mutation plan
    start_reporter = function(plan = NULL) {
      super$start_reporter(plan)
      self$mutants_by_file <- list()
      self$sources <- list()
    },

    #' @description Add a mutation test result
    #' @param plan Current testing plan. See [muttest_plan()].
    #' @param killed Whether the mutation was killed by tests
    #' @param survived Number of survived mutations
    #' @param no_coverage Number of mutants with no test coverage
    #' @param errors Number of errors encountered
    #' @param error Optional error condition from a failed run
    #' @param original_code Original source lines before mutation
    #' @param mutated_code Mutated source lines
    add_result = function(
      plan,
      killed,
      survived,
      no_coverage,
      errors,
      error = NULL,
      original_code = NULL,
      mutated_code = NULL
    ) {
      super$add_result(
        plan,
        killed,
        survived,
        no_coverage,
        errors,
        error,
        original_code,
        mutated_code
      )

      filename <- plan$filename
      mutator <- plan$mutator[[1]]
      mutation <- plan$mutation[[1]]
      status <- if (errors) {
        "RuntimeError"
      } else if (no_coverage) {
        "NoCoverage"
      } else if (killed) {
        "Killed"
      } else {
        "Survived"
      }

      self$sources[[filename]] <- self$sources[[filename]] %||% original_code
      name <- if (nzchar(mutator$to)) {
        paste(mutator$from, "\u2192", mutator$to)
      } else {
        mutator$from
      }
      loc <- mutation$location

      record <- list(
        id = sprintf("%d:%d:%s", loc$start$line, loc$start$column, name),
        mutatorName = name,
        replacement = mutation$replacement,
        location = loc,
        status = status
      )
      if (!is.null(error)) {
        record$statusReason <- conditionMessage(error)
      }

      self$mutants_by_file[[filename]] <- c(
        self$mutants_by_file[[filename]],
        list(record)
      )
    },

    #' @description End reporter, then write the JSON file
    end_reporter = function() {
      super$end_reporter()
      jsonlite::write_json(
        private$doc(),
        self$path,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        pretty = TRUE
      )
      rlang::inform(cli::col_grey(paste0("JSON report: ", self$path)))
    }
  ),
  private = list(
    # mutation-testing-elements schema (schemaVersion 1.0). Score and per-file
    # metrics are intentionally omitted -- consumers derive them from statuses.
    doc = function() {
      files <- lapply(names(self$mutants_by_file), function(path) {
        list(
          language = "r",
          source = paste(self$sources[[path]], collapse = "\n"),
          mutants = self$mutants_by_file[[path]]
        )
      })
      names(files) <- names(self$mutants_by_file)
      list(
        `$schema` = "https://raw.githubusercontent.com/stryker-mutator/mutation-testing-elements/master/packages/report-schema/src/mutation-testing-report-schema.json", # nolint
        schemaVersion = "1.0",
        thresholds = list(high = 80L, low = 50L),
        files = files
      )
    }
  )
)
