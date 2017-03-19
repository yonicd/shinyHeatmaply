server <- shinyServer(function(input, output,session) {	

data.sel=eventReactive(input$data,{
  if(input$data%in%d){
    eval(parse(text=paste0('data.in=as.data.frame(datasets::',input$data,')')))
    }else{
    data.in=read.csv(text=input$mydata[[input$data]])
  }
  data.in=as.data.frame(data.in)
  # data.in=data.in[,sapply(data.in,function(x) class(x))%in%c('numeric','integer')] # no need for this
  return(data.in)
})  

output$data=renderUI({
  selData='mtcars'
  if(!is.null(input$mydata)){
    d=c(names(input$mydata),d) 
    selData=tail(names(input$mydata),1)
  }
  selectInput("data","Select Data",d,selected = selData)
})

observeEvent(data.sel(),{
  output$annoVars<-renderUI({
    column(width=4,
           selectizeInput('annoVar','Annotation',choices = names(data.sel()),multiple=T)
    )
  })
})

output$colRng=renderUI({
  if(!is.null(data.sel())) {
    rng=range(data.sel(),na.rm = TRUE)
  }else{
    rng=range(mtcars) # TODO: this should probably be changed
  }
  # sliderInput("colorRng", "Set Color Range", min = round(rng[1],1), max = round(rng[2],1), step = .1, value = rng)  
  n_data = nrow(data.sel())
  min_range = rng[1]
  max_range = rng[2]
  a_good_step = 0.1 # (max_range-min_range) / n_data
  list(
    numericInput("colorRng_min", "Set Color Range (min)", value = min_range, min = -Inf, max = min_range, step = a_good_step),
    numericInput("colorRng_max", "Set Color Range (max)", value = max_range, min = max_range, max = Inf, step = a_good_step)
  )  
})


interactiveHeatmap<- reactive({
  data.in=data.sel()
  if(input$transpose) data.in=t(data.in)
  if(input$f!='.'){
    if(input$f=='is.na10') data.in=is.na10(data.in)
    if(input$f=='cor'){
      data.in=cor(data.in,use = "pairwise.complete.obs")
      updateSelectizeInput(session = session,inputId = 'pal',selected = "RdBu")
      updateNumericInput(session = session,inputId = 'colorRng_min',min=-1,max=1,value=-1)
      updateNumericInput(session = session,inputId = 'colorRng_max',min=-1,max=1,value=1)
      updateCheckboxInput(session=session,inputId = 'colRngAuto',value = F)
      
      updateCheckboxInput(session=session,inputId = 'showColor',value = T)
      updateCheckboxInput(session=session,inputId = 'colRngAuto',value = F)
    }
    if(input$f=='log') data.in=apply(data.in,2,function(x){
      x[x<=0]=1
      log(x) 
    })
    if(input$f=='sqrt') data.in=apply(data.in,2,sqrt)
    if(input$f=='normalize') data.in=apply(data.in,2,function(x) (x-min(x,na.rm = T))/max(x,na.rm = T))
    if(input$f=='scale') data.in=apply(data.in,2,function(x) (x-mean(x,na.rm = T))/var(x))
    if(input$f=='percentize') data.in=apply(data.in,2,function(x){
      f<-ecdf(x)
      f(x)
    })
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
  
  if(length(input$annoVar)>0) data.in=data.in%>%mutate_each_(funs(factor),input$annoVar)
  
  heatmaply(data.in,
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
    layout(margin = list(l = input$l, b = input$b))
    
})


observeEvent(input$data,{
output$heatout <- renderPlotly({
  if(!is.null(input$data))
    interactiveHeatmap()
})
})

observeEvent(input$mydata, {
  len = length(input$mydata)
  output$tables <- renderUI({
    table_list <- lapply(1:len, function(i) {
      tableName <- names(input$mydata)[[i]]
      tableOutput(tableName)
    })
    do.call(tagList, table_list)
  })
  # for (name in names(input$mydata)) {
  #   output[[name]] <- renderTable(read.csv(text=input$mydata[[name]]))
  # }
})


output$tables=renderDataTable(data.sel(),server = T,filter='top',
                              extensions = c('Scroller','FixedHeader','FixedColumns','Buttons','ColReorder'), options = list(
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

observeEvent({interactiveHeatmap()},{
  h<-interactiveHeatmap()
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("heatmaply-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      
      htmlwidgets::saveWidget(h,file)
    }
  )
})
})



