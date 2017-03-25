#'@title class_to
#'@description convert object to new class
#'@param x object
#'@param new_class new class to convert to
#'@keywords internal
#'@export
class_to <- function(x, new_class) {
  class(x) <- new_class
  x
}