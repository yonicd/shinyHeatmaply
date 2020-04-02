#'@title importSwitch
#'@description utility function to read imported data
#'@param file path to imported file
#'@return data.frame
#'@export
#'@keywords internal
#' @importFrom utils read.delim
#' @importFrom readxl read_excel
importSwitch=function(file){
  type=tolower(gsub('^(.*?)\\.','',file$name))
  
  if(type%in%c('csv','txt','tab')){
    sep=switch(type,
      csv=',',
      txt='\n',
      tab='\t'
    )
    out=utils::read.delim(file = file$datapath,stringsAsFactors = F,sep=sep)
  }
  
  if(type%in%c('rda','rd')){
    load(file$datapath)
    eval(parse(text=paste0('out=',ls(pattern = '[^type|^file]'))))
  }
  
  if(type%in%c('xls','xlsx')) out=readxl::read_excel(path=file$datapath)
  return(out)
  
}