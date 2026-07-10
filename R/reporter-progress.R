#' @title Progress Reporter for Mutation Testing
#'
#' @description
#' A reporter that displays a progress indicator for mutation tests.
#' It provides real-time feedback on which mutants are being tested and whether they were killed by tests.
#'
#' @field start_time Time when testing started (for duration calculation)
#' @field min_time Minimum test duration to display timing information
#' @field col_config List of column configuration for report formatting
#' @field survived_detail Controls how survived mutants are reported (summary, none)
#' @field survived_mutants List to store details of survived mutants for summary reporting
#'
#' @importFrom R6 R6Class
#' @importFrom cli col_green col_red col_yellow col_grey symbol
#' @md
#' @export
#' @family MutationReporter
ProgressMutationReporter <- R6::R6Class(
  classname = "ProgressMutationReporter",
  inherit = MutationReporter,
  public = list(
    start_time = NULL,
    min_time = 1,
    survived_detail = "summary",
    survived_mutants = list(),
    col_config = list(
      "status" = list(
        padding_left = 0,
        padding_right = 1,
        width = 2
      ),
      "k" = list(
        padding_left = 1,
        padding_right = 1,
        width = 5,
        type = "number"
      ),
      "s" = list(
        padding_left = 1,
        padding_right = 1,
        width = 5,
        type = "number"
      ),
      "n" = list(
        padding_left = 1,
        padding_right = 1,
        width = 5,
        type = "number"
      ),
      "e" = list(
        padding_left = 1,
        padding_right = 1,
        width = 5,
        type = "number"
      ),
      "t" = list(
        padding_left = 1,
        padding_right = 1,
        width = 5,
        type = "number"
      ),
      "score" = list(
        padding_left = 1,
        padding_right = 1,
        width = 5,
        type = "number"
      ),
      "mutator" = list(
        padding_left = 1,
        padding_right = 1,
        width = 10
      ),
      "file" = list(
        padding_left = 1,
        padding_right = 1
      )
    ),

    #' @description Format a column with specified padding and width
    #' @param text Text to format
    #' @param col_name Column name to use configuration from
    #' @param colorize Optional function to color the text
    format_column = function(text, col_name, colorize = NULL) {
      config <- self$col_config[[col_name]]

      # Get actual visible length of text
      text_len <- nchar(text)

      # If width is specified, calculate available text space after padding
      if (!is.null(config$width) && config$width > 0) {
        padding_size <- (config$padding_left %||% 0) +
          (config$padding_right %||% 0)
        text_max_width <- config$width - padding_size
        if (text_len > text_max_width && text_max_width > 0) {
          # Truncate if text is too long for the available space
          text <- substring(text, 1, text_max_width)
          text_len <- text_max_width
        }
      } else if (!is.null(config$max_width) && config$max_width > 0) {
        # Otherwise, use the existing max_width logic if provided
        text <- substring(text, 1, config$max_width)
        text_len <- min(text_len, config$max_width)
      }

      # Calculate left padding
      left_pad <- ""
      if (!is.null(config$padding_left) && config$padding_left > 0) {
        left_pad <- strrep(" ", config$padding_left)
      }

      # Calculate right padding
      right_pad <- ""
      if (!is.null(config$padding_right) && config$padding_right > 0) {
        right_pad <- strrep(" ", config$padding_right)
      }

      # Handle fixed width columns
      if (!is.null(config$width) && config$width > 0) {
        # Calculate current total width
        total_width <- text_len + nchar(left_pad) + nchar(right_pad)
        if (total_width < config$width) {
          # For number columns, add extra spaces to left padding for right alignment
          if (!is.null(config$type) && config$type == "number") {
            left_pad <- paste0(
              left_pad,
              strrep(" ", config$width - total_width)
            )
          } else {
            # Otherwise add to right padding (left alignment)
            right_pad <- paste0(
              right_pad,
              strrep(" ", config$width - total_width)
            )
          }
        }
      } else if (!is.null(config$max_width) && config$max_width > 0) {
        # Handle max_width padding (existing behavior)
        total_current_width <- text_len + nchar(left_pad) + nchar(right_pad)
        if (total_current_width < config$max_width) {
          # Add extra spaces to reach max width
          right_pad <- paste0(
            right_pad,
            strrep(" ", config$max_width - total_current_width)
          )
        }
      }

      # Apply color formatting after padding calculation but before concatenation
      if (!is.null(colorize) && is.function(colorize)) {
        text <- colorize(text)
      }

      paste0(left_pad, text, right_pad)
    },

    #' @description Format the header of the report
    fmt_h = function() {
      paste0(
        " ",
        " |",
        self$format_column("K", "k", cli::col_green),
        "|",
        self$format_column("S", "s", cli::col_red),
        "|",
        self$format_column("N", "n", cli::col_grey),
        "|",
        self$format_column("E", "e", cli::col_yellow),
        "|",
        self$format_column("T", "t"),
        "|",
        self$format_column("  %", "score"),
        "|",
        self$format_column("Mutator", "mutator"),
        "|",
        self$format_column("File", "file")
      )
    },

    #' @description Format a row of the report
    #' @param status Status symbol (e.g., tick or cross)
    #' @param k Number of killed mutations
    #' @param s Number of survived mutations
    #' @param n Number of mutations with no test coverage
    #' @param e Number of errors
    #' @param t Total number of mutations
    #' @param score Score percentage
    #' @param mutator The mutator used
    #' @param file The file being tested
    #' @return Formatted row string
    fmt_r = function(status, k, s, n, e, t, score, mutator, file) {
      paste0(
        status,
        " |",
        self$format_column(as.character(k), "k"),
        "|",
        self$format_column(as.character(s), "s"),
        "|",
        self$format_column(as.character(n), "n"),
        "|",
        self$format_column(as.character(e), "e"),
        "|",
        self$format_column(as.character(t), "t"),
        "|",
        self$format_column(score, "score"),
        "|",
        self$format_column(as.character(mutator), "mutator"),
        "|",
        self$format_column(file, "file")
      )
    },

    #' @description Initialize a new progress reporter
    #' @param test_reporter Reporter to use for testthat::test_dir
    #' @param min_time Minimum time to show elapsed time (default: 1s)
    #' @param file Output destination (default: stdout)
    #' @param survived_detail Controls how survived mutants are reported.
    #'   One of `"summary"` (default) or `"none"`.
    initialize = function(
      test_reporter = "silent",
      min_time = 1,
      file = stdout(),
      survived_detail = c("summary", "none")
    ) {
      super$initialize(test_reporter, file)

      self$min_time <- min_time
      self$survived_detail <- match.arg(survived_detail)
      self$results <- list()
    },

    #' @description Start reporter
    #' @param plan The complete mutation plan
    start_reporter = function(plan = NULL) {
      super$start_reporter(plan)
      self$start_time <- proc.time()
      self$results <- list()
      self$survived_mutants <- list()
      self$cat_line(paste(cli::symbol$info, "Mutation Testing"))
      self$cat_line(self$fmt_h())
    },

    #' @description Add a mutation test result
    #' @param plan Current testing plan. See `muttest_plan()`.
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
      super$add_result(plan, killed, survived, no_coverage, errors, error)

      status_symbol <- if (killed) {
        cli::col_green(cli::symbol$tick)
      } else if (no_coverage) {
        cli::col_grey("-")
      } else {
        cli::col_red("x")
      }

      filename <- plan$filename
      mutator <- plan$mutator[[1]]
      k <- self$results[[filename]]$killed
      s <- self$results[[filename]]$survived
      n <- self$results[[filename]]$no_coverage
      t <- self$results[[filename]]$total
      e <- self$results[[filename]]$errors
      file_name <- basename(filename)
      score <- if (is.na(self$current_score)) {
        "-"
      } else {
        floor(self$current_score * 100)
      }

      self$cat_line(self$fmt_r(
        status_symbol,
        k,
        s,
        n,
        e,
        t,
        score,
        info_oneline(mutator),
        file_name
      ))

      if (survived == 1 && !is.null(original_code) && !is.null(mutated_code)) {
        if (self$survived_detail == "summary") {
          self$survived_mutants <- c(
            self$survived_mutants,
            list(list(
              file_path = filename,
              mutator = mutator,
              original_code = original_code,
              mutated_code = mutated_code
            ))
          )
        }
      }
    },

    #' @description Update status spinner (for long-running operations)
    #' @param force Force update even if interval hasn't elapsed
    update = function(force = FALSE) {},

    #' @description End testing current file
    end_file = function() {
      super$end_file()
    },

    #' @description End reporter with detailed summary
    end_reporter = function() {
      self$cat_line()

      time <- proc.time() - self$start_time
      if (time[[3]] > self$min_time) {
        self$cat_line(cli::col_cyan(paste0(
          "Duration: ",
          sprintf("%.2f s", time[[3]])
        )))
      }

      private$cat_survived_mutants()

      private$cat_stats()
      self$cat_line()

      super$end_reporter()
    },

    #' @description Print the mutation test result with survived diffs
    print = function() {
      private$cat_stats()
      private$cat_survived_mutants()
      invisible(self)
    }
  ),
  private = list(
    cat_survived_mutants = function() {
      if (length(self$survived_mutants) == 0) {
        return(invisible(NULL))
      }
      self$cat_line()
      self$rule(cli::style_bold("Survived Mutants"))
      for (entry in self$survived_mutants) {
        private$print_survived_diff(
          entry$original_code,
          entry$mutated_code,
          entry$file_path,
          entry$mutator
        )
      }
    },

    print_survived_diff = function(
      original_code,
      mutated_code,
      file_path,
      mutator
    ) {
      changed <- which(original_code != mutated_code)
      if (length(changed) == 0) {
        return(invisible(NULL))
      }

      self$cat_line(cli::col_grey(paste0(
        basename(file_path),
        "  ",
        mutator$from,
        " \u2192 ",
        mutator$to
      )))
      for (i in changed) {
        self$cat_line(cli::col_red(paste0(
          "  ",
          i,
          "- ",
          trimws(original_code[[i]], "right")
        )))
        self$cat_line(cli::col_green(paste0(
          "  ",
          i,
          "+ ",
          trimws(mutated_code[[i]], "right")
        )))
      }
    },

    cat_stats = function() {
      results <- do.call(rbind, lapply(self$results, as.data.frame))
      k <- sum(results$killed)
      s <- sum(results$survived)
      n <- sum(results$no_coverage)
      t <- sum(results$total)
      e <- sum(results$errors)
      score <- self$current_score
      self$cat_line()
      self$rule(cli::style_bold("Results"))
      self$cat_line(
        "[ ",
        cli::col_green("KILLED "),
        k,
        " | ",
        cli::col_red("SURVIVED "),
        s,
        " | ",
        cli::col_grey("NO COVERAGE "),
        n,
        " | ",
        cli::col_yellow("ERRORS "),
        e,
        " | ",
        "TOTAL ",
        t,
        " | ",
        cli::style_bold(cli::col_green(sprintf("SCORE %.1f%%", score * 100))),
        " ]"
      )
    }
  )
)
