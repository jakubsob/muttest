get_repo <- function(url) {
  parts <- strsplit(url, "/")[[1]]
  zip <- parts[length(parts)]
  withr::with_dir(fs::path(tempdir()), {
    download.file(url = url, destfile = zip, quiet = TRUE)
    path <- unzip(zipfile = zip)
  })
  fs::path(tempdir(), fs::path_common(path))
}
