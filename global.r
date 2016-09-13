pkg=c('RColorBrewer','shiny','heatmaply','reshape2','ggplot2','plotly','stringr','plyr','dplyr')
sapply(pkg,function(x) require(x,warn.conflicts = F,quietly = T,character.only = T))

BrBG <- colorRampPalette(brewer.pal(11, "BrBG"))
Spectral <- colorRampPalette(brewer.pal(11, "Spectral"))

class_to <- function(x, new_class) {
  class(x) <- new_class
  x
}

na_mat <- function(x) {
  x %>% is.na %>% class_to("numeric") 
}