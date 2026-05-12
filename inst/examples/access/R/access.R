can_access <- function(is_admin, is_owner) {
  is_admin || is_owner
}
