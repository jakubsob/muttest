sign_of <- function(x) {
  if (x > 0) "positive" else if (x < 0) "negative" else "zero"
}

clamp <- function(x, lo, hi) {
  if (x < lo) return(lo)
  if (x > hi) return(hi)
  x
}
