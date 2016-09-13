server <- shinyServer(function(input, output,session) {	
  
  
interactiveHeatmap<- reactive({
  eval(parse(text=paste0('data.in=datasets::',input$data)))
  if(input$f!='.') eval(parse(text=paste0('data.in=',input$f,'(data.in)')))
  heatmaply(data.in,
            seriate = input$seration,
            colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
            k_col = input$c, 
            k_row = input$r,
            limits = c(-1,1)) %>% 
    layout(margin = list(l = input$l, b = input$b))
})


output$heatout <- renderPlotly({
  if(!is.null(input$data))
    interactiveHeatmap()
})
})