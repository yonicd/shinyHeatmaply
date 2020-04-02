#'@title html2tagList
#'@description convert raw html to tagList
#'@param x character vector of html
#'@examples 
#'x<-'<h1>Title</h1>
#'    <h2>Header text</h2>
#'    <p>Text here</p>
#'    <h1>Title</h1>
#'    <h2>Header text</h2>
#'    <p>Text here</p>'
#'    
#'  html2tagList(x)  
#'
#'require(xtable)
#'htmlIn<-print(xtable::xtable(mtcars),type = 'html',print.results = FALSE)
#'htmlIn
#'tagL<-html2tagList(htmlIn)
#'class(tagL)
#'tagL
#'if(interactive()) htmltools::browsable(tagL)
#'@export
#'@keywords internal
#'@import htmltools
html2tagList<-function(x){
  
  x<-strsplit(gsub('>','>_AAA_',x),'_AAA_')[[1]]
  
  #remove any \\n 
    x<-gsub('\\n','',x)
  #remove comments  
    x<-x[!grepl('<!',x)]
  #replace " with '
    x<-gsub('"',"'",x)
  
  #replace close tag with ),  
    x<-gsub('</(.*?)>',"),",x)
  
  #Attributted tags
    attrIdx=grep('=',x)
    attrVal=grep('=',x,value = T)
    attrVal=gsub('>',",",attrVal)
    attrVal=gsub('<','tags$',attrVal)
    attrVal=sub('^ ','',attrVal)
    attrVal=sub(' ',"(",attrVal)
    x[attrIdx]=attrVal
  
  #Non Attributed tags
    x=gsub('<','tags$',x)
    x=gsub('>',"(",x)
    x=sub('^ ','',x)
    x=sub(' )',')',x)
  
  #add quotes in tags
    endIdx=grep('),',x)
    endVal=grep('),',x,value = T)
    endVal=paste0("'",gsub('),',"'),",endVal))
    x[endIdx]=endVal
  
  #remove last comma from collapsed string
    xout=paste0(x,collapse = '')
    xout=gsub(',$','',xout)
  #eval to tagList object
    xout<-eval(parse(text=sprintf('htmltools::tagList(list(%s))',xout),keep.source = TRUE))
  return(xout)
}
