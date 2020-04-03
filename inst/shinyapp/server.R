d=data(package='datasets')$results[,'Item']
d=d[!grepl('[\\()]',d)]
d=d[!d%in%c('UScitiesD','eurodist','sleep','warpbreaks')]
d=d[unlist(lapply(d,function(d.in) eval(parse(text=paste0('ncol(as.data.frame(datasets::',d.in,'))')))))>1]
d=d[-which(d=='mtcars')]
d=c('mtcars',d)

server <- shinyServer(function(input, output,session) {
  TEMPLIST<-new.env()
  TEMPLIST$d<-d
#Annotation Variable UI ----
observeEvent(data.sel(),{
  output$annoVars<-renderUI({
    data.in=data.sel()
    NM=NULL
    
    if(any(sapply(data.in,class)=='factor')){
      NM=names(data.in)[which(sapply(data.in,class)=='factor')]  
    } 
    column(width=4,
           selectizeInput('annoVar','Annotation',choices = names(data.in),selected=NM,multiple=T,options = list(placeholder = 'select columns',plugins = list("remove_button")))
    )
  })

#Sampling UI ----  
  output$sample<-renderUI({
    list(
      column(4,textInput(inputId = 'setSeed',label = 'Seed',value = sample(1:10000,1))),
      column(4,numericInput(inputId = 'selRows',label = 'Number of Rows',min=1,max=pmin(500,nrow(data.sel())),value = pmin(500,nrow(data.sel())))),
      column(4,selectizeInput('selCols','Columns Subset',choices = names(data.sel()),multiple=T))
    )
  })
})

#Data Selection UI ----
output$data=renderUI({
  if(!is.null(input$mydata)) TEMPLIST$d=c(input$mydata$name,TEMPLIST$d)
  selData=head(TEMPLIST$d,1)
  selectInput("data","Select Data",TEMPLIST$d,selected = selData)
})


#Color Pallete UI ----
output$colUI<-renderUI({
  
  colSel='Vidiris'
  if(input$transform_fun=='cor') colSel='RdBu'
  if(input$transform_fun=='is.na10') colSel='grey.colors'
  
  selectizeInput(inputId ="pal", label ="Select Color Palette",
                 choices = c('Vidiris (Sequential)'="viridis",
                             'Magma (Sequential)'="magma",
                             'Plasma (Sequential)'="plasma",
                             'Inferno (Sequential)'="inferno",
                             'Magma (Sequential)'="magma",
                             'Magma (Sequential)'="magma",
                             
                             'RdBu (Diverging)'="RdBu",
                             'RdYlBu (Diverging)'="RdYlBu",
                             'RdYlGn (Diverging)'="RdYlGn",
                             'BrBG (Diverging)'="BrBG",
                             'Spectral (Diverging)'="Spectral",
                             
                             'BuGn (Sequential)'='BuGn',
                             'PuBuGn (Sequential)'='PuBuGn',
                             'YlOrRd (Sequential)'='YlOrRd',
                             'Heat (Sequential)'='heat.colors',
                             'Grey (Sequential)'='grey.colors'),
                 selected=colSel)
})

#Manual Color Range UI ----
output$colRng=renderUI({
  if(!is.null(data.sel())) {
    rng=range(data.sel(),na.rm = TRUE)
  }else{
    rng=range(mtcars) # TODO: this should probably be changed
  }
  # sliderInput("colorRng", "Set Color Range", min = round(rng[1],1), max = round(rng[2],1), step = .1, value = rng)  
  n_data = nrow(data.sel())
  
  min_min_range = ifelse(input$transform_fun=='cor',-1,-Inf)
  min_max_range = ifelse(input$transform_fun=='cor',1,rng[1])
  min_value = ifelse(input$transform_fun=='cor',-1,rng[1])
  
  max_min_range = ifelse(input$transform_fun=='cor',-1,rng[2])
  max_max_range = ifelse(input$transform_fun=='cor',1,Inf)
  max_value = ifelse(input$transform_fun=='cor',1,rng[2])
  
  a_good_step = 0.1 # (max_range-min_range) / n_data
  
  list(
    numericInput("colorRng_min", "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
    numericInput("colorRng_max", "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
  )
  
})

#Import/Select Data ----
data.sel=eventReactive(input$data,{
  if(input$data%in%d){
    eval(parse(text=paste0('data.in=as.data.frame(datasets::',input$data,')')))
  }else{
    data.in=importSwitch(input$mydata[input$mydata$name%in%input$data,])
  }
  data.in=as.data.frame(data.in)
  # data.in=data.in[,sapply(data.in,function(x) class(x))%in%c('numeric','integer')] # no need for this
  return(data.in)
})  

#Building heatmaply ----
interactiveHeatmap<- reactive({
  data.in=data.sel()
  if(input$showSample){
    if(!is.null(input$selRows)){
        set.seed(input$setSeed)
      if((input$selRows >= 2) & (input$selRows < nrow(data.in))){
        # if input$selRows == nrow(data.in) then we should not do anything (this save refreshing when clicking the subset button)
        if(length(input$selCols)<=1) data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRows)),]
        if(length(input$selCols)>1) data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRows)),input$selCols]
      }
    }
  }
  # ss_num = sapply(data.in,function(x) class(x)) %in% c('numeric','integer') # in order to only transform the numeric values
  
  if(length(input$annoVar)>0){
    if(all(input$annoVar%in%names(data.in))) 
      data.in <- data.in%>%mutate_at(funs(factor),.vars=vars(input$annoVar))
  } 
  
  ss_num =  sapply(data.in, is.numeric) # in order to only transform the numeric values

  if(input$transpose) data.in=t(data.in)
  if(input$transform_fun!='.'){
    if(input$transform_fun=='is.na10'){
      updateCheckboxInput(session = session,inputId = 'showColor',value = T)
      data.in[, ss_num]=is.na10(data.in[, ss_num])
    } 
    if(input$transform_fun=='cor'){
      updateCheckboxInput(session = session,inputId = 'showColor',value = T)
      updateCheckboxInput(session = session,inputId = 'colRngAuto',value = F)
      data.in=cor(data.in[, ss_num],use = "pairwise.complete.obs")
    }
    if(input$transform_fun=='log') data.in[, ss_num]= apply(data.in[, ss_num],2,log)
    if(input$transform_fun=='sqrt') data.in[, ss_num]= apply(data.in[, ss_num],2,sqrt) 
    if(input$transform_fun=='normalize') data.in=heatmaply::normalize(data.in)
    if(input$transform_fun=='scale') data.in[, ss_num] = scale(data.in[, ss_num])
    if(input$transform_fun=='percentize') data.in=heatmaply::percentize(data.in)
  } 
  

      
  if(!is.null(input$tables_true_search_columns)) 
    data.in=data.in[activeRows(input$tables_true_search_columns,data.in),]
  if(input$colRngAuto){
    ColLimits=NULL 
  }else{
    ColLimits=c(input$colorRng_min, input$colorRng_max)
  }
  
  distfun_row = function(x) dist(x, method = input$distFun_row)
  distfun_col =  function(x) dist(x, method = input$distFun_col)
  
  hclustfun_row = function(x) hclust(x, method = input$hclustFun_row)
  hclustfun_col = function(x) hclust(x, method = input$hclustFun_col)
  
  p <- heatmaply(data.in,
            main = input$main,xlab = input$xlab,ylab = input$ylab,
            row_text_angle = input$row_text_angle,
            column_text_angle = input$column_text_angle,
            dendrogram = input$dendrogram,
            branches_lwd = input$branches_lwd,
            seriate = input$seriation,
            colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
            distfun_row =  distfun_row,
            hclustfun_row = hclustfun_row,
            distfun_col = distfun_col,
            hclustfun_col = hclustfun_col,
            k_col = input$c, 
            k_row = input$r,
            limits = ColLimits) %>% 
    layout(margin = list(l = input$l, b = input$b, r='0px'))
    
  p$elementId <- NULL
  
  p
})

#Render Plot ----
observeEvent(input$data,{
output$heatout <- renderPlotly({
  if(!is.null(input$data))
    interactiveHeatmap()
})
})

#Render Data Table ----
output$tables=DT::renderDataTable(data.sel(),server = T,filter='top',
                              extensions = c('Scroller','FixedHeader','FixedColumns','Buttons','ColReorder'),
                              options = list(
                                dom = 't',
                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),
                                colReorder = TRUE,
                                scrollX = TRUE,
                                fixedColumns = TRUE,
                                fixedHeader = TRUE,
                                deferRender = TRUE,
                                scrollY = 500,
                                scroller = TRUE
                              ))

#Clone Heatmap ----
observeEvent({interactiveHeatmap()},{
  h<-interactiveHeatmap()
  
  l<-list(main = input$main,xlab = input$xlab,ylab = input$ylab,
          row_text_angle = input$row_text_angle,
          column_text_angle = input$column_text_angle,
          dendrogram = input$dendrogram,
          branches_lwd = input$branches_lwd,
          seriate = input$seriation,
          colors=paste0(input$pal,'(',input$ncol,')'),
          distfun_row =  input$distFun_row,
          hclustfun_row = input$hclustFun_row,
          distfun_col = input$distFun_col,
          hclustfun_col = input$hclustFun_col,
          k_col = input$c, 
          k_row = input$r,
          limits = paste(c(input$colorRng_min, input$colorRng_max),collapse=',')
  )
  
  #l=l[!l=='']
  l=data.frame(Parameter=names(l),Value=do.call('rbind',l),row.names = NULL,stringsAsFactors = F)
  l[which(l$Value==''),2]='NULL'
  paramTbl=print(xtable::xtable(l),type = 'html',include.rownames=FALSE,print.results = F,html.table.attributes = c('border=0'))
  
  
  h$width='100%'
  h$height='800px'
  s<-tags$div(style="position: relative; bottom: 5px;",
              HTML(paramTbl),
                tags$em('This heatmap visualization was created using',
                  tags$a(href="https://github.com/yonicd/shinyHeatmaply/",target="_blank",'shinyHeatmaply'),
                  Sys.time()
                        )
              )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("heatmaply-", strftime(Sys.time(),'%Y%m%d_%H%M%S'), ".html")
    },
    content = function(file) {
      libdir <- paste0(tools::file_path_sans_ext(basename(file)),"_files")

      htmltools::save_html(htmltools::browsable(htmltools::tagList(h,s)),file=file,libdir = libdir)
      if (!htmlwidgets:::pandoc_available()) {
          stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
          "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
      }
      
      htmlwidgets:::pandoc_self_contained_html(file, file)
      
      unlink(libdir, recursive = TRUE)
    }
  )
})
#End of Code ----
})



