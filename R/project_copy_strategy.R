#' Project Copy Strategy
#'
#' @export
ProjectCopyStrategy <- R6::R6Class(
  classname = "ProjectCopyStrategy",
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

#' StandardProjectCopyStrategy class
#' @export
StandardProjectCopyStrategy <- R6::R6Class(
  classname = "StandardProjectCopyStrategy",
  inherit = ProjectCopyStrategy,
  public = list(
    #' @description
    #' Copy project files, excluding hidden and temp directories
    #'
    #' @param original_dir The original directory to copy from
    #' @param plan The current test plan
    #' @return The path to the temporary directory
    execute = function(original_dir, plan) {
      temp_dir <- fs::path(tempdir(), digest::digest(plan))

      # Get directories to copy (exclude hidden, temp directories)
      dirs_to_copy <- list.dirs(original_dir, recursive = FALSE, full.names = FALSE)
      dirs_to_copy <- dirs_to_copy[!grepl("^\\.|tmp|temp", dirs_to_copy)]

      # Use purrr::walk to copy directories
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

      temp_dir
    }
  )
)

#' Create a default project copy strategy
#'
#' @param ... Unused, saved for future expansion.
#' @return A ProjectCopyStrategy object
#' @export
default_project_copy_strategy <- function(...) {
  StandardProjectCopyStrategy$new(...)
}
