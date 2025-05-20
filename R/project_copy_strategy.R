#' @title CopyStrategy interface
#'
#' @description
#' Extend this class to implement a custom copy strategy.
#'
#' @md
#' @export
#' @family CopyStrategy
CopyStrategy <- R6::R6Class(
  classname = "CopyStrategy",
  public = list(
    #' @description
    #' Copy project files according to the strategy
    #'
    #' @param original_dir The original directory to copy from
    #' @param plan The current test plan
    #' @return The path to the temporary directory
    execute = function(original_dir) {
      rlang::abort("Not implemented")
    }
  )
)

#' @title Package copy strategy
#'
#' @description
#' It copies all files and directories from the original directory to a temporary directory.
#'
#' @md
#' @export
#' @family CopyStrategy
PackageCopyStrategy <- R6::R6Class(
  classname = "PackageCopyStrategy",
  inherit = CopyStrategy,
  public = list(
    #' @description
    #' Copy project files, excluding hidden and temp directories
    #'
    #' @param original_dir The original directory to copy from
    #' @param plan The current test plan
    #' @return The path to the temporary directory
    execute = function(original_dir, plan) {
      temp_dir <- fs::path(tempdir(), digest::digest(plan))

      dirs_to_copy <- list.dirs(
        original_dir,
        recursive = FALSE,
        full.names = FALSE
      )
      dirs_to_copy <- dirs_to_copy[!grepl("^\\.|tmp|temp", dirs_to_copy)]

      purrr::walk(dirs_to_copy, function(dir) {
        src_path <- file.path(original_dir, dir)
        if (dir.exists(src_path)) {
          fs::dir_copy(
            src_path,
            file.path(temp_dir, dir),
            overwrite = TRUE
          )
        }
      })

      files <- fs::dir_ls(original_dir, type = "file")
      files <- fs::path_rel(files, original_dir)
      purrr::walk(files, function(x) {
        fs::file_copy(
          x,
          file.path(temp_dir, x),
          overwrite = TRUE
        )
      })

      temp_dir
    }
  )
)

#' Create a default project copy strategy
#'
#' @param ... Arguments passed to the `?PackageCopyStrategy` constructor.
#' @return A `?CopyStrategy` object
#' @md
#' @export
#' @family CopyStrategy
default_copy_strategy <- function(...) {
  PackageCopyStrategy$new(...)
}
