#'@title launch_heatmaply
#'@description launch shiny app UI for heatmaply from R console
#'@param obj data.frame or list of data.frames
#'@param plotHeight numeric that sets the height of the plot output (default 800px)
#'@param viewerType character of the viewer to be used to launch the app to c('paneViewer','dialogViewer','browserViewer')
#'@examples
#' \dontrun{
#'if(interactive()){
#'data(mtcars)
#'launch_heatmaply(mtcars)
#'
#'data(iris)
#'launch_heatmaply(list('Example1'=mtcars,'Example2'=iris))
#'}
#'}
#'@export
launch_heatmaply=function(obj,plotHeight=800,viewerType='paneViewer'){
  if(!'list'%in%class(obj)) obj=list(obj)
  if(is.null(names(obj))) names(obj)=paste0('data',seq(1,length(obj)))
  heatmaplyGadget(obj,plotHeight,viewerType)
}