#' Build a source-annotated HTML report from a muttest JSON file
#'
#' Reads a JSON file produced by [JSONMutationReporter] (the
#' \href{https://github.com/stryker-mutator/mutation-testing-elements}{mutation-testing-elements}
#' schema) and renders a self-contained HTML report: overall score, per-file
#' breakdown, and every mutant overlaid on its source line. Mutated lines
#' expand to show each mutation as a diff. A small inline script adds status
#' filtering (the report opens focused on survived mutants) and `n`/`p`
#' keyboard navigation between mutated lines.
#'
#' The report is a single static file. Syntax highlighting is loaded from a CDN
#' (\href{https://shiki.style}{shiki}) when online and is skipped gracefully
#' offline.
#'
#' @param json Path to the JSON file written by [JSONMutationReporter].
#' @param output Path of the HTML file to write. Defaults to the JSON path with
#'   an `.html` extension.
#' @return The `output` path, invisibly.
#' @export
#' @md
report <- function(
  json = "muttest.json",
  output = sub("\\.json$", ".html", json)
) {
  checkmate::assert_file_exists(json)
  doc <- jsonlite::read_json(json)

  counts <- .status_counts(unlist(lapply(doc$files, function(f) {
    vapply(f$mutants, function(m) .css_status(m$status), character(1))
  })))
  thresholds <- doc$thresholds %||% list(high = 80, low = 50)

  body <- htmltools::div(
    class = "wrap",
    .report_header(counts, thresholds),
    .report_plan(doc),
    lapply(names(doc$files), function(path) {
      .report_file(path, doc$files[[path]])
    })
  )

  # The report opens filtered to survived mutants (the actionable ones);
  # fall back to showing everything when nothing survived.
  init <- if (counts$survived > 0) "survived" else "all"

  # htmltools strips <head> from a manually-built <html> tag (it reserves head
  # for its dependency machinery), so assemble the document shell by hand and
  # only render the dynamic body with htmltools.
  writeLines(
    c(
      "<!doctype html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"utf-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      "<title>muttest report</title>",
      "<style>",
      readLines(system.file("report", "muttest.css", package = "muttest")),
      "</style>",
      "</head>",
      sprintf("<body data-f=\"%s\">", init),
      as.character(body),
      "<script>",
      readLines(system.file("report", "muttest.js", package = "muttest")),
      "</script>",
      "<script type=\"module\">",
      readLines(system.file("report", "muttest-shiki.mjs", package = "muttest")),
      "</script>",
      "</body>",
      "</html>"
    ),
    output
  )
  invisible(output)
}

# Standard status vocabulary -> the CSS/JS class names used by the report.
.css_status <- function(status) {
  map <- c(Killed = "killed", Survived = "survived", NoCoverage = "no_coverage")
  out <- unname(map[status])
  out[is.na(out)] <- "error"
  out
}

.status_counts <- function(css_statuses) {
  n <- function(st) sum(css_statuses == st)
  list(
    killed = n("killed"),
    survived = n("survived"),
    no_coverage = n("no_coverage"),
    error = n("error"),
    total = length(css_statuses)
  )
}

# Detected (killed + errored) as a share of scored (killed + survived + error)
# mutants, or NULL when nothing was scored. An erroring mutant was still
# detected, so it counts as killed.
.score <- function(counts) {
  scored <- counts$killed + counts$survived + counts$error
  if (scored == 0) NULL else (counts$killed + counts$error) / scored
}

.report_header <- function(counts, thresholds) {
  score <- .score(counts)
  pct <- if (is.null(score)) {
    htmltools::HTML("&ndash;")
  } else {
    paste0(floor(score * 100), "%")
  }
  # Score bands get their own classes so the grade palette (green/amber/red) is
  # decoupled from the per-mutant status colors it happens to share the hue with.
  score_class <- if (is.null(score)) {
    "band-none"
  } else if (score >= thresholds$high / 100) {
    "band-good"
  } else if (score >= thresholds$low / 100) {
    "band-mid"
  } else {
    "band-low"
  }
  chip <- function(f, label, n, hint) {
    htmltools::tags$button(
      class = paste("chip", f),
      `data-f` = f,
      `aria-pressed` = "false",
      `data-tip` = hint,
      disabled = if (n == 0) NA,
      paste0(label, " "),
      htmltools::tags$b(n)
    )
  }
  htmltools::tagList(
    htmltools::div(
      class = "mast",
      htmltools::h1("muttest report"),
      htmltools::div(class = paste("score", score_class), pct)
    ),
    htmltools::div(
      class = "toolbar",
      chip("all", "All", counts$total, "show every mutant"),
      chip(
        "survived",
        "Survived",
        counts$survived,
        "tests missed these: start here"
      ),
      chip(
        "killed",
        "Killed",
        counts$killed,
        "tests caught these: no action needed"
      ),
      chip(
        "no_coverage",
        "No coverage",
        counts$no_coverage,
        "no test exercised this code"
      ),
      chip("error", "Errors", counts$error, "mutant failed to run"),
      htmltools::div(
        class = "tools",
        htmltools::tags$span(
          class = "hint",
          htmltools::tags$kbd("n"),
          " / ",
          htmltools::tags$kbd("p"),
          " to jump between mutants"
        ),
        htmltools::tags$select(
          class = "tbtn fjump",
          id = "filejump",
          `aria-label` = "Jump to file",
          htmltools::tags$option(value = "", "Jump to file")
        ),
        htmltools::tags$button(
          class = "tbtn ibtn",
          `data-open` = "1",
          `data-tip` = "Expand all",
          `aria-label` = "Expand all",
          htmltools::HTML(
            paste0(
              '<svg viewBox="0 0 16 16" width="14" height="14" fill="none" stroke="currentColor" ',
              'stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round">',
              '<path d="M4 6l4-4 4 4"/><path d="M4 10l4 4 4-4"/></svg>'
            )
          )
        ),
        htmltools::tags$button(
          class = "tbtn ibtn",
          `data-open` = "0",
          `data-tip` = "Collapse all",
          `aria-label` = "Collapse all",
          htmltools::HTML(
            paste0(
              '<svg viewBox="0 0 16 16" width="14" height="14" fill="none" stroke="currentColor" ',
              'stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round">',
              '<path d="M4 2l4 4 4-4"/><path d="M4 14l4-4 4 4"/></svg>'
            )
          )
        ),
        htmltools::tags$button(
          class = "tbtn ibtn",
          id = "theme",
          `aria-label` = "Theme"
        )
      ),
      # Filter scope indicator; JS fills it and it wraps to its own row below
      # the chips (flex-basis:100%), keeping the chips coupled to what they hide.
      htmltools::tags$span(class = "fcount", id = "fcount")
    )
  )
}

# The executed plan: every mutator that was applied, grouped by name, with its
# kill breakdown and score. All derivable from the mutants already in `doc`.
.report_plan <- function(doc) {
  mutants <- unlist(lapply(doc$files, function(f) f$mutants), recursive = FALSE)
  names <- vapply(mutants, function(m) m$mutatorName, character(1))
  statuses <- vapply(mutants, function(m) .css_status(m$status), character(1))
  by <- split(statuses, names)

  rows <- lapply(names(by), function(nm) {
    c <- .status_counts(by[[nm]])
    list(name = nm, counts = c, score = .score(c))
  })
  # worst-scoring mutators first (most actionable); unscored last
  rows <- rows[order(vapply(rows, function(r) r$score %||% Inf, numeric(1)))]

  cell <- function(n, cls) {
    if (n == 0) {
      htmltools::tags$td(class = "z", 0)
    } else {
      htmltools::tags$td(class = cls, n)
    }
  }
  # Tag with the statuses actually present so the status filter never hides the
  # plan (it summarizes mutants of every present status).
  # ponytail: reuses details.file styling; own class only if plan needs to diverge.
  # Collapsed by default so the chips sit directly above the file list they
  # filter; the overview is one click away.
  htmltools::tags$details(
    class = paste(
      c("file plan", paste0("has-", unique(statuses))),
      collapse = " "
    ),
    htmltools::tags$summary(
      htmltools::tags$span(class = "fname", "Mutation plan"),
      htmltools::tags$span(
        class = "fmeta",
        htmltools::tags$span(
          class = "ct",
          htmltools::tags$b(length(by)),
          " mutators"
        )
      )
    ),
    htmltools::tags$table(
      class = "plan-tbl",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Mutator"),
        htmltools::tags$th("Killed"),
        htmltools::tags$th("Survived"),
        htmltools::tags$th("No coverage"),
        htmltools::tags$th("Errors"),
        htmltools::tags$th("Total"),
        htmltools::tags$th("Score")
      )),
      htmltools::tags$tbody(lapply(rows, function(r) {
        htmltools::tags$tr(
          htmltools::tags$td(class = "mname", r$name),
          cell(r$counts$killed, "killed"),
          cell(r$counts$survived, "survived"),
          cell(r$counts$no_coverage, "no_coverage"),
          cell(r$counts$error, "error"),
          htmltools::tags$td(r$counts$total),
          htmltools::tags$td(
            if (is.null(r$score)) {
              htmltools::HTML("&ndash;")
            } else {
              paste0(floor(r$score * 100), "%")
            }
          )
        )
      }))
    )
  )
}

.report_file <- function(path, file) {
  source <- strsplit(file$source, "\n", fixed = TRUE)[[1]]
  statuses <- vapply(file$mutants, function(m) .css_status(m$status), character(1))
  counts <- .status_counts(statuses)
  score <- .score(counts)
  fscore <- if (is.null(score)) htmltools::HTML("&ndash;") else paste0(floor(score * 100), "%")
  count <- function(st, label) {
    if (counts[[st]] == 0) {
      return(NULL)
    }
    htmltools::tags$span(
      class = "ct",
      htmltools::tags$b(class = st, counts[[st]]),
      paste0(" ", label)
    )
  }

  # group mutants by the line where their mutation begins
  by_line <- list()
  for (m in file$mutants) {
    ln <- as.character(m$location$start$line)
    by_line[[ln]] <- c(by_line[[ln]], list(m))
  }

  rows <- lapply(seq_along(source), function(i) {
    .report_line(source, i, by_line[[as.character(i)]])
  })

  htmltools::tags$details(
    class = paste(c("file", paste0("has-", unique(statuses))), collapse = " "),
    htmltools::tags$summary(
      htmltools::tags$span(class = "fname", path),
      htmltools::tags$span(
        class = "fmeta",
        count("survived", "survived"),
        count("killed", "killed"),
        count("no_coverage", "no coverage"),
        count("error", if (counts$error == 1) "error" else "errors"),
        htmltools::tags$span(
          class = "fscore",
          "score ",
          htmltools::tags$b(fscore)
        )
      )
    ),
    htmltools::div(class = "src", rows)
  )
}

.report_line <- function(source, no, hits) {
  text <- source[[no]]
  num <- htmltools::tags$span(class = "no", no)
  txt <- if (nzchar(text)) text else htmltools::HTML("&nbsp;")

  if (is.null(hits)) {
    return(htmltools::div(
      class = "ln",
      num,
      htmltools::tags$span(class = "txt", txt)
    ))
  }

  statuses <- vapply(hits, function(m) .css_status(m$status), character(1))
  htmltools::tags$details(
    class = paste(
      c("ln hit", paste0("b-", .worst(statuses)), paste0("has-", unique(statuses))),
      collapse = " "
    ),
    htmltools::tags$summary(
      num,
      htmltools::tags$span(class = "txt", txt),
      htmltools::tags$span(
        class = "badge",
        paste(length(hits), if (length(hits) == 1) "mutant" else "mutants")
      )
    ),
    htmltools::div(class = "muts", lapply(hits, function(m) .report_mutant(source, m)))
  )
}

.report_mutant <- function(source, m) {
  st <- .css_status(m$status)
  d <- .apply_mutation(source, m$location, m$replacement)
  diff <- htmltools::tagList(
    lapply(d$original, function(l) htmltools::tags$span(class = "del", paste0("- ", l))),
    lapply(d$mutated, function(l) htmltools::tags$span(class = "add", paste0("+ ", l)))
  )
  htmltools::div(
    class = paste0("mut m-", st),
    htmltools::div(
      class = "head",
      htmltools::tags$span(class = paste("st", st), gsub("_", " ", st)),
      htmltools::tags$span(class = "rule", m$mutatorName)
    ),
    htmltools::tags$code(diff),
    if (!is.null(m$statusReason)) htmltools::tags$code(class = "msg", m$statusReason)
  )
}

# Reconstruct the original and mutated line text for a mutant from the source,
# its location (1-based, end-exclusive), and its replacement -- mirrors the
# splice done by the mutation engine.
.apply_mutation <- function(source, location, replacement) {
  s <- location$start
  e <- location$end
  span <- source[s$line:e$line]
  prefix <- substr(span[1], 1, s$column - 1)
  last <- span[length(span)]
  suffix <- substr(last, e$column, nchar(last))
  merged <- paste0(prefix, replacement, suffix)
  # A deletion leaves only surrounding whitespace -- show it as a removed line
  # (no mutated side) rather than a blank "+" line.
  list(
    original = as.list(span),
    mutated = if (grepl("\\S", merged)) as.list(strsplit(merged, "\n", fixed = TRUE)[[1]]) else list()
  )
}

# Worst (most attention-worthy) status wins the line marker.
.worst <- function(statuses) {
  order <- c(survived = 0, error = 1, no_coverage = 2, killed = 3)
  names(which.min(order[statuses]))
}
