testthat::skip_on_os("windows")

# The report inlines its CSS/JS to be self-contained; that would bloat the
# snapshot. Replace each inline block with a link to the real asset in
# inst/report/, relative to the snapshot's location (tests/testthat/_snaps/report-html/)
link_inline_assets <- function(lines) {
  base <- "../../../../inst/report"
  links <- c(
    "<style>" = sprintf('<link rel="stylesheet" href="%s/muttest.css">', base),
    "<script>" = sprintf('<script src="%s/muttest.js"></script>', base),
    '<script type="module">' =
      sprintf('<script type="module" src="%s/muttest-shiki.mjs"></script>', base)
  )
  in_asset <- FALSE
  out <- character()
  for (l in lines) {
    if (grepl("^</style>|^</script>", l)) {
      in_asset <- FALSE
      next
    }
    if (in_asset) next
    if (l %in% names(links)) {
      out <- c(out, links[[l]])
      in_asset <- TRUE
    } else {
      out <- c(out, l)
    }
  }
  out
}

test_that("report renders the JSON into a static page", {
  .with_example_dir("shipping/", {
    # Arrange
    json <- withr::local_tempfile(fileext = ".json")
    html <- withr::local_tempfile(fileext = ".html")
    mutators <- list(operator(">", "<"), operator(">", ">="))
    plan <- muttest_plan(mutators, fs::dir_ls("R"))
    suppressMessages(muttest(
      plan,
      reporter = JSONMutationReporter$new(path = json)
    ))

    # Act
    report(json, output = html)

    # Assert
    expect_snapshot_file(
      html, "muttest-report.html",
      transform = link_inline_assets
    )
  })
})
