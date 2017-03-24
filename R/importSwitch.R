#'@export
importSwitch=function(file){
  type=tolower(gsub('^(.*?)\\.','',file$name))
  
  if(type%in%c('csv','txt','tab')){
    sep=switch(type,
      csv=',',
      txt='\n',
      tab='\t'
    )
    out=read.delim(file = file$datapath,stringsAsFactors = F,sep=sep)
  }
  
  if(type%in%c('rda','rd')){
    load(file$datapath)
    eval(parse(text=paste0('out=',ls(pattern = '[^type|^file]'))))
  }
  
  if(type%in%c('xls','xlsx')) out=read_excel(path=file$datapath)
  return(out)
  
}