#' Run a mutation test
#'
#' @param plan A mutation testing plan. See [muttest_plan()].
#' @param path Path to the test directory.
#' @param reporter Reporter to use for mutation testing results. See [MutationReporter].
#' @param test_strategy Strategy for running tests. See [TestStrategy].
#'   The purpose of test strategy is to control how tests are executed.
#'   We can run all tests for each mutant, or only tests that are relevant to the mutant.
#' @param copy_strategy Strategy for copying the project. See [CopyStrategy].
#'   This strategy controls which files are copied to the temporary directory, where the tests are run.
#' @param workers Number of parallel workers. When greater than 1, mutants are tested
#'   concurrently using `mirai` daemons. Defaults to 1 (sequential).
#' @param timeout Per-mutant timeout in milliseconds. If a mutant's test run exceeds this
#'   limit the daemon is interrupted and the result is recorded as an error.
#'   Use `Inf` to disable. Defaults to 600000 (10 minutes).
#'
#' @return An object of class `muttest_result` containing the overall mutation score.
#'
#' @export
#' @md
muttest <- function(
  plan,
  path = "tests/testthat",
  reporter = default_reporter(),
  test_strategy = default_test_strategy(),
  copy_strategy = default_copy_strategy(),
  workers = 1,
  timeout = 600 * 1000
) {
  checkmate::assert_directory_exists(path)
  checkmate::assert(
    checkmate::check_multi_class(plan, "muttest_plan"),
    checkmate::check_data_frame(plan),
    checkmate::check_set_equal(
      c("filename", "original_code", "mutated_code", "mutator", "mutation"),
      names(plan)
    ),
    combine = "and"
  )
  checkmate::assert_class(reporter, "MutationReporter")
  checkmate::assert_class(test_strategy, "TestStrategy", null.ok = TRUE)
  checkmate::assert_class(copy_strategy, "CopyStrategy")
  checkmate::assert_count(workers, positive = TRUE)
  checkmate::assert_number(timeout, lower = 0)

  if (nrow(plan) == 0) {
    return(invisible(NA_real_))
  }

  reporter$start_reporter(plan)

  mutator_key <- vapply(plan$mutator, function(m) m$from, character(1))
  plan <- plan[order(plan$filename, mutator_key), ]
  rows <- lapply(seq_len(nrow(plan)), function(i) plan[i, , drop = FALSE])

  mirai::daemons(workers)
  on.exit(mirai::daemons(0), add = TRUE)

  wd <- getwd()
  test_reporter <- reporter$test_reporter
  timeout_ms <- if (is.finite(timeout)) as.integer(timeout) else NULL

  tasks <- lapply(rows, function(row) {
    reporter$start_file(row$filename)
    reporter$start_mutator(row$mutator[[1]])
    reporter$update(force = TRUE)

    filename <- row$filename
    mutated_code <- row$mutated_code[[1]]

    # Pass only plain serializable data - row$mutator contains treesitter
    # C-level objects that cannot cross process boundaries.
    mirai::mirai(
      {
        # Reconstruct a minimal row so strategies can access $filename
        minimal_row <- data.frame(
          filename = filename,
          mutated_code = I(list(mutated_code)),
          stringsAsFactors = FALSE
        )
        dir <- copy_strategy$execute(wd, minimal_row)
        on.exit(fs::dir_delete(dir))
        test_results <- tryCatch(
          withr::with_tempdir(tmpdir = dir, pattern = "", {
            withr::with_dir(dir, {
              writeLines(mutated_code, file.path(dir, filename))
              test_strategy$execute(path, minimal_row, test_reporter)
            })
          }),
          error = function(e) e
        )
        list(test_results = test_results)
      },
      copy_strategy = copy_strategy,
      test_strategy = test_strategy,
      wd = wd,
      filename = filename,
      mutated_code = mutated_code,
      path = path,
      test_reporter = test_reporter,
      .timeout = timeout_ms
    )
  })

  for (i in seq_along(tasks)) {
    res <- tasks[[i]][] # blocks until the task resolves
    row <- rows[[i]]
    if (mirai::is_error_value(res) && !mirai::is_mirai_error(res)) {
      .record_result(
        reporter,
        row,
        simpleError("Timed out"),
        row$mutated_code[[1]]
      )
    } else if (mirai::is_error_value(res)) {
      .record_result(
        reporter,
        row,
        simpleError(format(res)),
        row$mutated_code[[1]]
      )
    } else {
      .record_result(reporter, row, res$test_results, row$mutated_code[[1]])
    }
  }

  reporter$end_reporter()
  invisible(muttest_result(reporter))
}

muttest_result <- function(reporter) {
  structure(
    reporter$get_score(),
    reporter = reporter,
    class = c("muttest_result", "numeric")
  )
}

#' @export
print.muttest_result <- function(x, ...) {
  attr(x, "reporter")$print()
  invisible(x)
}

#' Record a single mutant's outcome with the reporter
#'
#' Outcomes are mutually exclusive per mutant: `killed + survived + no_coverage
#' + errors == 1`. A run-level crash or timeout is an error; an empty result
#' means no test exercised the mutant (no coverage); a test failure means the
#' mutation was detected (killed); a test error means the mutation broke the
#' function itself and is recorded as an error (not a kill), unless a failure
#' also occurred.
#'
#' @param reporter The mutation reporter to record the result with.
#' @param row The plan row for the current mutant.
#' @param test_results Test results, an error condition, or an empty result.
#' @param mutated_code The mutated source lines.
#' @noRd
.record_result <- function(reporter, row, test_results, mutated_code) {
  if (inherits(test_results, "error")) {
    outcome <- list(killed = 0, survived = 0, no_coverage = 0, errors = 1)
    error <- test_results
  } else if (length(test_results) == 0) {
    outcome <- list(killed = 0, survived = 0, no_coverage = 1, errors = 0)
    error <- NULL
  } else {
    df <- as.data.frame(test_results)
    if (sum(df$failed) > 0) {
      outcome <- list(killed = 1, survived = 0, no_coverage = 0, errors = 0)
    } else if (sum(df$error) > 0) {
      outcome <- list(killed = 0, survived = 0, no_coverage = 0, errors = 1)
    } else {
      outcome <- list(killed = 0, survived = 1, no_coverage = 0, errors = 0)
    }
    error <- NULL
  }
  reporter$add_result(
    row,
    killed = outcome$killed,
    survived = outcome$survived,
    no_coverage = outcome$no_coverage,
    errors = outcome$errors,
    error = error,
    original_code = row$original_code[[1]],
    mutated_code = mutated_code
  )
  reporter$end_mutator()
  reporter$end_file()
}

#' Create a plan for mutation testing
#'
#' Each mutant requires rerunning the tests. For large project it might be not feasible to test all
#' mutants in one go. This function allows you to create a plan for selected source files and mutators.
#'
#' The plan is in a data frame format, where each row represents a mutant.
#'
#' You can subset the plan before passing it to the [muttest()] function.
#'
#' @param mutators A list of mutators to use. See [operator()].
#' @param source_files A vector of file paths to the source files.
#' @return A data frame with the test plan.
#'   The data frame has the following columns:
#'   - `filename`: The name of the source file.
#'   - `original_code`: The original code of the source file.
#'   - `mutated_code`: The mutated code of the source file.
#'   - `mutator`: The mutator that was applied.
#'   - `mutation`: A list with the mutant's `location` (1-based `start`/`end`
#'     line/column) and `replacement` text.
#'
#' @export
#' @md
muttest_plan <- function(
  mutators,
  source_files = fs::dir_ls("R", regexp = ".[rR]$")
) {
  checkmate::assert_file_exists(source_files, extension = c("R", "r"))
  checkmate::assert_list(mutators)
  rows <- list()
  for (mutator in mutators) {
    for (filename in source_files) {
      code_lines <- readLines(filename)
      mutations <- mutator$mutate(code_lines)
      for (mutation in mutations) {
        row <- data.frame(
          filename = filename,
          original_code = I(list(code_lines)),
          mutated_code = I(list(mutation$code)),
          mutator = I(list(mutator)),
          mutation = I(list(mutation[c("location", "replacement")])),
          stringsAsFactors = FALSE
        )
        rows <- c(rows, list(row))
      }
    }
  }
  if (length(rows) == 0) {
    return(.muttest_plan(data.frame(
      filename = character(),
      original_code = I(list()),
      mutated_code = I(list()),
      mutator = I(list()),
      mutation = I(list()),
      stringsAsFactors = FALSE
    )))
  }
  .muttest_plan(do.call(rbind, rows))
}

.muttest_plan <- function(x) {
  structure(x, class = c("muttest_plan", class(x)))
}

#' @export
print.muttest_plan <- function(x, ..., nrows = 10) {
  n_mutants <- nrow(x)
  n_files <- length(unique(x$filename))

  cli::cat_rule(cli::style_bold("Mutation Test Plan"))
  cli::cat_line(sprintf(
    "%d %s across %d %s",
    n_mutants,
    ngettext(n_mutants, "mutant", "mutants"),
    n_files,
    ngettext(n_files, "file", "files")
  ))
  cli::cat_line()

  shown <- x[seq_len(min(nrows, n_mutants)), ]
  for (i in seq_len(nrow(shown))) {
    m <- shown$mutator[[i]]
    cli::cat_line(paste0(
      cli::col_grey(shown$filename[[i]]),
      "  ",
      cli::style_bold(m$from),
      paste0(" ", SYMBOLS$arrow, " "),
      cli::style_bold(m$to)
    ))
  }

  if (n_mutants > nrows) {
    cli::cat_line()
    cli::cat_line(cli::col_grey(sprintf(
      "... and %d more %s",
      n_mutants - nrows,
      ngettext(n_mutants - nrows, "mutant", "mutants")
    )))
  }

  invisible(x)
}
