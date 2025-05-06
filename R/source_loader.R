#' @inherit default_source_loader
#' @export
#' @family source_loaders
null_loader <- function(...) {
  function(dir) {
    new.env()
  }
}

#' @inherit default_source_loader
#' @export
#' @family source_loaders
dir_loader <- function(...) {
  function(dir) {
    files <- fs::dir_ls(fs::path(dir, "R"), regexp = "\\.[rR]$")
    env <- new.env()
    purrr::walk(files, source, local = env)
    env
  }
}

#' Default source loader
#'
#' @param ... Unused, saved for future expansion.
#' @return A function that loads source files from a directory.
#'
#' @export
#' @family source_loaders
default_source_loader <- function(...) {
  dir_loader()
}
