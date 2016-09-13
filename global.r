# pkg=c('RColorBrewer','shiny','heatmaply','reshape2','ggplot2','plotly','stringr','plyr','dplyr')
# sapply(pkg,function(x) require(x,warn.conflicts = F,quietly = T,character.only = T))

library(RColorBrewer)
library(shiny)
library(heatmaply)
library(dplyr)

BrBG <- colorRampPalette(brewer.pal(11, "BrBG"))
Spectral <- colorRampPalette(brewer.pal(11, "Spectral"))

class_to <- function(x, new_class) {
  class(x) <- new_class
  x
}

na_mat <- function(x,...) {
  x %>% is.na %>% class_to("numeric") 
}

d=data(package='datasets')$results[,'Item']
d=d[!grepl('[\\()]',d)]
d=d[!d%in%c('UScitiesD','eurodist')]
d=d[unlist(lapply(d,function(d.in) eval(parse(text=paste0('ncol(as.data.frame(datasets::',d.in,'))')))))>1]

