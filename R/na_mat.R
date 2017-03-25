#'@title na_mat
#'@description convert to numeric if there is NA in object
#'@param x object
#'@keywords internal
#'@export
na_mat <- function(x,...) {
x %>% is.na %>% class_to("numeric") 
}