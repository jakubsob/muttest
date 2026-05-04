greet <- function(name) {
  if (nchar(name) == 0) return("Hello, stranger!")
  paste0("Hello, ", name, "!")
}

default_label <- function(label) {
  if (label == "") "unknown" else label
}
