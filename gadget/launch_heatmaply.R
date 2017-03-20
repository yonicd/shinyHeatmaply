launch_heatmaply=function(obj,minHeight=1000){
  if(!'list'%in%class(obj)) obj=list(obj)
  if(is.null(names(obj))) names(obj)=paste0('data',seq(1,length(obj)))
  heatmaplyGadget(obj,minHieght)
}