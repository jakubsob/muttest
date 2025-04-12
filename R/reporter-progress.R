#' Progress Reporter for Mutation Testing
#'
#' A reporter that displays a progress indicator for mutation tests.
#' It provides real-time feedback on which mutations are being tested
#' and whether they were killed by tests.
#'
#' @field frames Spinner animation frames for dynamic display
#' @field dynamic Whether dynamic output is supported in the current terminal
#' @field start_time Time when testing started (for duration calculation)
#' @field min_time Minimum test duration to display timing information
#' @field last_update Time of the last progress update
#' @field update_interval How often to update the spinner (in seconds)
#' @field frame_index Current frame index in the spinner animation
#' @field mutations_by_file List tracking mutation statistics by file
#' @field table_header_printed Whether the table header has been printed
#' @field current_mutation_index List tracking current mutation index by file
#' @field col_status_width Width for status column
#' @field col_killed_width Width for killed mutations column
#' @field col_survived_width Width for survived mutations column
#' @field col_total_width Width for total mutations column
#' @field col_file_width Width for file name column
#' @field col_sep Column separator string
#'
#' @importFrom R6 R6Class
#' @importFrom cli col_green col_red col_yellow col_grey symbol
#' @export
#' @family reporters
MutationProgressReporter <- R6::R6Class("MutationProgressReporter",
  inherit = MutationReporter,
  public = list(
    # For dynamic display
    frames = NULL,
    dynamic = FALSE,
    start_time = NULL,
    min_time = 1,
    last_update = NULL,
    update_interval = 0.1,
    frame_index = 0,

    # Track mutations by file
    mutations_by_file = list(),

    # For tabular output
    table_header_printed = FALSE,
    current_mutation_index = list(),

    # Column width configuration
    col_status_width = 3,
    col_killed_width = 5,
    col_survived_width = 5,
    col_total_width = 5,
    col_file_width = 20,
    col_sep = " | ",

    #' @description Initialize a new progress reporter
    #' @param test_reporter Reporter to use for testthat::test_dir
    #' @param min_time Minimum time to show elapsed time (default: 1s)
    #' @param update_interval How often to update the display (default: 0.1s)
    #' @param file Output destination (default: stdout)
    initialize = function(test_reporter = "silent",
                          min_time = 1,
                          update_interval = 0.1,
                          file = stdout()) {
      super$initialize(test_reporter, file)

      self$min_time <- min_time
      self$update_interval <- update_interval
      self$mutations_by_file <- list()
      self$current_mutation_index <- list()
      self$table_header_printed <- FALSE

      self$frames <- cli::get_spinner()$frames
      self$dynamic <- cli::is_dynamic_tty()
    },

    #' @description Start reporter
    #' @param plan The complete mutation plan
    #' @param temp_dir Path to the temporary directory for testing
    start_reporter = function(plan = NULL, temp_dir = NULL) {
      super$start_reporter(plan, temp_dir)
      self$start_time <- proc.time()
      self$mutations_by_file <- list()
      self$current_mutation_index <- list()
      self$table_header_printed <- FALSE
      self$rule("Mutation Testing", line = 1)

      self$cat_line("Using temporary directory: ", self$temp_dir)

      total_mutations <- nrow(self$plan)
      unique_files <- length(unique(self$plan$file_path))
      unique_mutators <- length(unique(sapply(self$plan$mutator, function(m) paste(m$from, "->", m$to))))

      self$cat_line("Starting mutation testing with:")
      self$cat_line("  - ", total_mutations, " total mutations")
      self$cat_line("  - ", unique_files, " files to mutate")
      self$cat_line("  - ", unique_mutators, " unique mutation operators")

      self$cat_line()
    },

    #' @description Start testing a file
    #' @param file_path Path to the file being mutated
    start_file = function(file_path) {
      super$start_file(file_path)

      # Initialize mutation tracking for this file
      if (is.null(self$mutations_by_file[[file_path]])) {
        self$mutations_by_file[[file_path]] <- list(
          total = 0,
          killed = 0,
          survived = 0
        )

        # Reset mutation index counter for this file
        self$current_mutation_index[[file_path]] <- 0
      }

      # Print table header if this is the first file or mutation
      if (!self$table_header_printed) {
        # Format header with colored K and S
        self$cat_line(
          "   |  ",
          col_green("K"), "    ",
          col_red("S"), "    T   | File"
        )
        self$table_header_printed <- TRUE
      }
    },

    #' @description Start testing with a specific mutator
    #' @param mutator The mutator being applied
    start_mutator = function(mutator) {
      super$start_mutator(mutator)
      # We don't need to print anything here, will show in add_result
    },

    #' @description Add a mutation test result
    #' @param file_path Path to the file that was mutated
    #' @param mutator The mutator that was applied
    #' @param test_results Results from the test execution
    #' @param killed Whether the mutation was killed by tests
    add_result = function(file_path, mutator, test_results, killed) {
      super$add_result(file_path, mutator, test_results, killed)

      self$mutations_by_file[[file_path]]$total <- self$mutations_by_file[[file_path]]$total + 1
      if (killed) {
        self$mutations_by_file[[file_path]]$killed <- self$mutations_by_file[[file_path]]$killed + 1
      } else {
        self$mutations_by_file[[file_path]]$survived <- self$mutations_by_file[[file_path]]$survived + 1
      }

      # Increment mutation index for this file
      self$current_mutation_index[[file_path]] <- self$current_mutation_index[[file_path]] + 1
      mutation_idx <- self$current_mutation_index[[file_path]]

      # Format table values with proper spacing
      status_symbol <- if (killed) col_green(cli::symbol$tick) else col_red("x")
      k <- sprintf("%3d", self$mutations_by_file[[file_path]]$killed)
      s <- sprintf("%3d", self$mutations_by_file[[file_path]]$survived)
      t <- sprintf("%3d", self$mutations_by_file[[file_path]]$total)

      file_name <- basename(file_path)
      file_name <- sprintf("%-15s", substring(file_name, 1, 15))

      mutation_desc <- sprintf("%d \"%s\" -> \"%s\"",
                              mutation_idx,
                              mutator$from,
                              mutator$to)

      # Use carriage return to overwrite the spinner line
      # nocov start
      if (self$dynamic) {
        # Clear line completely and write the result
        self$cat_tight("\r\033[K")
      }
      # nocov end

      # Format table row to match the requested example exactly
      self$cat_line(
        status_symbol, "  |",
        k, "  ", s, "  ", t, "   | ",
        file_name, " ",
        mutation_desc
      )
    },

    #' @description Update status spinner (for long-running operations)
    #' @param force Force update even if interval hasn't elapsed
    update = function(force = FALSE) {
      now <- proc.time()[[3]]
      if (force || is.null(self$last_update) ||
          now - self$last_update > self$update_interval) {

        self$last_update <- now

        # nocov start
        if (self$dynamic) {
          self$frame_index <- (self$frame_index %% length(self$frames)) + 1
          frame <- self$frames[self$frame_index]

          # Show temporary spinner that will be completely replaced by add_result
          # Use a clear format that matches the add_result output structure
          self$cat_tight(
            "\r", frame, "  |               |                       | Running mutation..."
          )
        }
        # nocov end
      }
    },

    #' @description End testing current file
    end_file = function() {
      # We don't need to print anything here since the tabular format shows file progress
      super$end_file()
    },

    #' @description Carriage return if dynamic, newline otherwise
    # nocov start
    cr = function() {
      if (self$dynamic) {
        "\r"
      } else {
        "\n"
      }
    },
    # nocov end

    #' @description End reporter with detailed summary
    end_reporter = function() {
      # Clear any progress display
      # nocov start
      if (self$dynamic) {
        self$cat_tight(self$cr(), strrep(" ", self$width))
      }
      # nocov end

      self$cat_line()

      # End time statistics
      # nocov start
      time <- proc.time() - self$start_time
      if (time[[3]] > self$min_time) {
        self$cat_line(col_cyan(paste0("Duration: ", sprintf("%.2f s", time[[3]]))))
        self$cat_line()
      }
      # nocov end

      # Show mutation distribution by file
      if (self$total_mutations > 0) {
        self$rule("Mutation Distribution", line = 1)

        # Display stats for each file
        for (file_path in names(self$mutations_by_file)) {
          stats <- self$mutations_by_file[[file_path]]
          file_score <- if (stats$total > 0) stats$killed / stats$total else 1.0

          self$cat_line(
            basename(file_path), ": ",
            col_green(paste0(stats$killed, " killed")), " / ",
            col_red(paste0(stats$survived, " survived")), " / ",
            stats$total, " total (", sprintf("%.1f%%", file_score * 100), ")"
          )
        }

        self$cat_line()
      }

      super$end_reporter()
    }
  )
)
