# Custom knitr engines for rendering muttest examples.
# Source in a vignette setup chunk: source("engine.R")
#
# Two engines are registered:
#   muttest_example  — chunk body = one example name, renders that example
#   muttest_examples — chunk body is empty (or comma-separated filter), renders all matching examples

.examples_root <- function() {
  # Prefer source tree so development and pkgdown builds use current files,
  # not a potentially stale installed copy.
  from_vignettes <- file.path("..", "inst", "examples")
  if (dir.exists(from_vignettes)) return(normalizePath(from_vignettes))
  from_root <- file.path("inst", "examples")
  if (dir.exists(from_root)) return(normalizePath(from_root))
  installed <- system.file("examples", package = "muttest")
  if (nzchar(installed)) return(installed)
  stop("Cannot locate inst/examples/")
}

.example_path <- function(name) {
  file.path(.examples_root(), name)
}

# Parse a README.md into a named list of section contents.
# Returns: list with $title (from first # heading) and named elements for each ## section.
.parse_readme <- function(path) {
  lines <- readLines(path, warn = FALSE)

  title_idx <- grep("^# ", lines)
  title <- if (length(title_idx) > 0) sub("^# ", "", lines[title_idx[[1]]]) else ""

  section_idx <- grep("^## ", lines)
  sections <- list(title = title)

  for (i in seq_along(section_idx)) {
    start <- section_idx[[i]]
    end <- if (i < length(section_idx)) section_idx[[i + 1]] - 1L else length(lines)
    name <- sub("^## ", "", lines[[start]])
    content <- trimws(paste(lines[seq(start + 1L, end)], collapse = "\n"))
    sections[[name]] <- content
  }

  sections
}

# Read all R files in a directory, concatenated.
.read_dir_r <- function(dir_path) {
  files <- sort(list.files(dir_path, pattern = "\\.R$", full.names = TRUE))
  if (length(files) == 0) return("")
  # Strip the source() call that examples use for dev-mode sourcing
  lines <- unlist(lapply(files, function(f) {
    raw <- readLines(f, warn = FALSE)
    raw[!grepl("^source\\(file\\.path\\(", raw)]
  }))
  # Strip only empty lines at start and end, but preserve internal spacing
  lines <- sub("^[\r\n]+", "", paste(lines, collapse = "\n"))
  paste(lines, collapse = "\n")
}

# Run muttest on one example with a given test directory, capturing plain text output.
.run_example <- function(example_path, test_dir) {
  withr::with_options(
    list(cli.num_colors = 0, cli.unicode = TRUE, width = 80),
    withr::with_dir(example_path, {
      p <- local(source("plan.R", local = TRUE)$value)
      lines <- capture.output(
        suppressMessages(
          muttest::muttest(
            p,
            path = test_dir,
            reporter = muttest::default_reporter(
              survived_detail = "none",
              min_time = Inf
            )
          )
        )
      )
      paste(lines, collapse = "\n")
    })
  )
}

# Render a single example to a markdown string.
.render_example <- function(name) {
  path <- .example_path(name)
  readme <- .parse_readme(file.path(path, "README.md"))

  source_code <- .read_dir_r(file.path(path, "R"))
  weak_code <- .read_dir_r(file.path(path, "tests", "testthat"))
  strong_code <- .read_dir_r(file.path(path, "tests", "testthat-fix"))

  weak_output <- .run_example(path, file.path("tests", "testthat"))
  strong_output <- .run_example(path, file.path("tests", "testthat-fix"))

  section <- function(key) if (is.null(readme[[key]])) "" else readme[[key]]

  paste(
    c(
      paste0("### Example: ", readme$title),
      "",
      section("What this demonstrates"),
      "",
      "#### The function",
      "",
      "```r",
      source_code,
      "```",
      "",
      section("The function"),
      "",
      "#### Weak test",
      "",
      "```r",
      weak_code,
      "```",
      "",
      section("The weak test"),
      "",
      "**Mutation testing output:**",
      "",
      "```",
      weak_output,
      "```",
      "",
      section("The surviving mutant"),
      "",
      "#### The fix",
      "",
      "```r",
      strong_code,
      "```",
      "",
      section("The fix"),
      "",
      "**After the fix:**",
      "",
      "```",
      strong_output,
      "```",
      "",
      section("Key rule")
    ),
    collapse = "\n"
  )
}

knitr::knit_engines$set(muttest_example = function(options) {
  name <- trimws(options$code)
  knitr::asis_output(.render_example(name))
})

knitr::knit_engines$set(muttest_examples = function(options) {
  filter <- trimws(options$code)

  root <- .examples_root()
  all_names <- sort(list.dirs(root, full.names = FALSE, recursive = FALSE))
  all_names <- all_names[file.exists(file.path(root, all_names, "plan.R"))]

  if (nzchar(filter)) {
    requested <- trimws(strsplit(filter, ",")[[1]])
    all_names <- all_names[all_names %in% requested]
  }

  output <- paste(vapply(all_names, .render_example, character(1)), collapse = "\n\n---\n\n")
  knitr::asis_output(output)
})
