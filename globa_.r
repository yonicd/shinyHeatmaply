# library(shinyHeatmaply)
# d=data(package='datasets')$results[,'Item']
# d=d[!grepl('[\\()]',d)]
# d=d[!d%in%c('UScitiesD','eurodist','sleep','warpbreaks')]
# d=d[unlist(lapply(d,function(d.in) eval(parse(text=paste0('ncol(as.data.frame(datasets::',d.in,'))')))))>1]