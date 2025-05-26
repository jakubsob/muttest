get_repo <- function(url) {
  parts <- strsplit(url, "/")[[1]]
  zip <- parts[length(parts)]
  withr::with_dir(fs::path(tempdir()), {
    tryCatch(
      download.file(url = url, destfile = zip, quiet = TRUE),
      error = function(e) {
        testthat::skip(paste("Failed to download repository from", url))
      }
    )
    path <- unzip(zipfile = zip)
  })
  fs::path(tempdir(), fs::path_common(path))
}
