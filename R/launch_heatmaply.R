#'@title launch_heatmaply
#'@description launch shiny app UI for heatmaply from R console
#'@param obj data.frame or list of data.frames
#'@param minHeight numeric that gets passed to paneViewer
#'@examples
#'if(interactive()){
#'data(mtcars)
#'launch_heatmaply(mtcars)
#'
#'data(iris)
#'launch_heatmaply(list('Example1'=mtcars,'Example2'=iris))
#'}
#'@export
launch_heatmaply=function(obj,minHeight=1000){
  if(!'list'%in%class(obj)) obj=list(obj)
  if(is.null(names(obj))) names(obj)=paste0('data',seq(1,length(obj)))
  heatmaplyGadget(obj,minHieght)
}