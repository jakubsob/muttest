is_valid <- function(x) {
  if (is.null(x)) return(FALSE)
  TRUE
}

all_valid <- function(xs) {
  ok <- TRUE
  for (x in xs) {
    if (!is_valid(x)) ok <- FALSE
  }
  ok
}
