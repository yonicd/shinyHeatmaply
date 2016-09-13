server <- shinyServer(function(input, output,session) {	

data.sel=reactive({
  if(input$data%in%d){
    eval(parse(text=paste0('data.in=as.data.frame(datasets::',input$data,')')))
    }else{
    data.in=read.csv(text=input$mydata[[input$data]])
  }
  if(input$f!='.') eval(parse(text=paste0('data.in=',input$f,'(data.in,use = "pairwise.complete.obs")')))
  data.in=as.data.frame(data.in)
  return(data.in)
})  

output$data=renderUI({
  if(!is.null(input$mydata)) d=c(d,names(input$mydata))
  selectInput("data","Select Data",d,selected = 'mtcars')
})

output$colRng=renderUI({
  if(!is.null(data.sel())) {
    rng=range(data.sel()[,sapply(data.sel(),function(x) class(x))=='numeric'],na.rm = T)
  }else{
    rng=range(mtcars)
  }
  sliderInput("colorRng", "Set Color Range", min = round(rng[1],1), max = round(rng[2],1), step = .1, value = rng)  
})

  
interactiveHeatmap<- reactive({
  data.in=data.sel()
  heatmaply(data.in,
            seriate = input$seration,
            colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
            k_col = input$c, 
            k_row = input$r,
            limits = input$colorRng) %>% 
    layout(margin = list(l = input$l, b = input$b))
})


output$heatout <- renderPlotly({
  if(!is.null(input$data))
    interactiveHeatmap()
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

output$tables=renderTable(data.sel())

})