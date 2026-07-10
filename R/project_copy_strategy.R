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
#' When `symlink = TRUE`, only the top-level directory holding the mutated file
#' (e.g. `R/`) is copied; every other top-level directory and root file is
#' symlinked to the original. For N mutants this avoids N full copies, which
#' matters when the project carries large `inst/`/`data/` directories. Tests
#' only read the symlinked files, so this is safe as long as the test suite
#' does not write into the package's own directories (writes through a symlink
#' would modify the original).
#'
#' @md
#' @export
#' @family CopyStrategy
#' @importFrom digest digest
PackageCopyStrategy <- R6::R6Class(
  classname = "PackageCopyStrategy",
  inherit = CopyStrategy,
  private = list(
    symlink = FALSE
  ),
  public = list(
    #' @description
    #' Create a package copy strategy
    #'
    #' @param symlink If `TRUE`, symlink unchanged files instead of copying them.
    initialize = function(symlink = FALSE) {
      private$symlink <- symlink
    },
    #' @description
    #' Copy project files, excluding hidden and temp directories
    #'
    #' @param original_dir The original directory to copy from
    #' @param plan The current test plan
    #' @return The path to the temporary directory
    execute = function(original_dir, plan) {
      temp_dir <- fs::path(tempdir(), digest::digest(plan))
      fs::dir_create(temp_dir)

      # The top-level entry holding the file we overwrite must be a real copy;
      # writing through a symlink would clobber the original source.
      mutated_top <- if (private$symlink && length(plan$filename)) {
        fs::path_split(plan$filename)[[1]][1]
      }
      place <- function(src, dst, is_dir) {
        if (private$symlink && !identical(fs::path_file(dst), mutated_top)) {
          fs::link_create(fs::path_abs(src), dst)
        } else if (is_dir) {
          fs::dir_copy(src, dst, overwrite = TRUE)
        } else {
          fs::file_copy(src, dst, overwrite = TRUE)
        }
      }

      dirs_to_copy <- list.dirs(
        original_dir,
        recursive = FALSE,
        full.names = FALSE
      )
      dirs_to_copy <- dirs_to_copy[!grepl("^\\.|tmp|temp", dirs_to_copy)]
      lapply(dirs_to_copy, function(dir) {
        src_path <- fs::path(original_dir, dir)
        if (dir.exists(src_path)) {
          place(src_path, fs::path(temp_dir, dir), is_dir = TRUE)
        }
      })

      files <- fs::path_file(fs::dir_ls(original_dir, type = "file"))
      lapply(files, function(f) {
        place(fs::path(original_dir, f), fs::path(temp_dir, f), is_dir = FALSE)
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
