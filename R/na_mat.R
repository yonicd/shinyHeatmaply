#'@export
na_mat <- function(x,...) {
x %>% is.na %>% class_to("numeric") 
}