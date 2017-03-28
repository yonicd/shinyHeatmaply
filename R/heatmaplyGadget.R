heatmaplyGadget<-function(obj,plotHeight=800,viewerType='paneViewer',...){
viewerDots<-list(...) 

if(viewerType=='dialogViewer'){
  if(is.null(viewerDots$dialogName)) viewerDots$dialogName='shinyHeatmaply'
  if(is.null(viewerDots$width)) viewerDots$width=1600
  if(is.null(viewerDots$height)) viewerDots$height=1000
} 

viewer<-do.call(eval(parse(text=paste0('shiny::',viewerType))),viewerDots)
#UI----
  ui <- shiny::shinyUI(
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          htmltools::h4('Data'),
          shiny::uiOutput('data'),
          shiny::checkboxInput('showSample','Subset Data'),
          shiny::conditionalPanel('input.showSample',shiny::uiOutput('sample')),
          # br(),
          htmltools::hr(),htmltools::h4('Data Preprocessing'),
          shiny::column(width=4,shiny::selectizeInput('transpose','Transpose',choices = c('No'=FALSE,'Yes'=TRUE),selected = FALSE)),
          shiny::column(width=4,shiny::selectizeInput("transform_fun", "Transform", c(Identity=".",Sqrt='sqrt',log='log',Scale='scale',Normalize='normalize',Percentize='percentize',"Missing values"='is.na10', Correlation='cor'),selected = '.')),
          shiny::uiOutput('annoVars'),
          
          htmltools::br(),htmltools::hr(),htmltools::h4('Row dendrogram'),
          shiny::column(width=6,shiny::selectizeInput("distFun_row", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
          shiny::column(width=6,shiny::selectizeInput("hclustFun_row", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
          shiny::column(width=12,shiny::sliderInput("r", "Number of Clusters", min = 1, max = 15, value = 2)),    
          #column(width=4,numericInput("r", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),   
          
          htmltools::br(),htmltools::hr(),htmltools::h4('Column dendrogram'),
          shiny::column(width=6,shiny::selectizeInput("distFun_col", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
          shiny::column(width=6,shiny::selectizeInput("hclustFun_col", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
          shiny::column(width=12,shiny::sliderInput("c", "Number of Clusters", min = 1, max = 15, value = 2)),
          #column(width=4,numericInput("c", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),    
          
          htmltools::br(),htmltools::hr(),  htmltools::h4('Additional Parameters'),
          
          shiny::column(3,shiny::checkboxInput('showColor','Color')),
          shiny::column(3,shiny::checkboxInput('showMargin','Layout')),
          shiny::column(3,shiny::checkboxInput('showDendo','Dendrogram')),
          htmltools::hr(),
          shiny::conditionalPanel('input.showColor==1',
                           htmltools::hr(),
                           htmltools::h4('Color Manipulation'),
                           shiny::uiOutput('colUI'),
                           shiny::sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
                           shiny::checkboxInput('colRngAuto','Auto Color Range',value = T),
                           shiny::conditionalPanel('!input.colRngAuto',shiny::uiOutput('colRng'))
          ),
          
          shiny::conditionalPanel('input.showDendo==1',
                           htmltools::hr(),
                           htmltools::h4('Dendrogram Manipulation'),
                           shiny::selectInput('dendrogram','Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both'),
                           shiny::selectizeInput("seriation", "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
                           shiny::sliderInput('branches_lwd','Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1)
          ),             
          
          shiny::conditionalPanel('input.showMargin==1',
                           htmltools::hr(),
                           htmltools::h4('Widget Layout'),
                           shiny::column(4,shiny::textInput('main','Title','')),
                           shiny::column(4,shiny::textInput('xlab','X Title','')),
                           shiny::column(4,shiny::textInput('ylab','Y Title','')),
                           shiny::sliderInput('row_text_angle','Row Text Angle',value = 0,min=0,max=180),
                           shiny::sliderInput('column_text_angle','Column Text Angle',value = 45,min=0,max=180),
                           shiny::sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
                           shiny::sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40)
          )
        ),
        
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Heatmaply",
                     htmltools::tags$a(id = 'downloadData', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, shiny::icon("clone"), 'Download Heatmap as HTML'),
                     htmltools::tags$head(htmltools::tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
                     plotly::plotlyOutput("heatout",height=paste0(plotHeight,'px'))
            ),
            shiny::tabPanel("Data",
                     DT::dataTableOutput('tables')
            )
          ) 
        )
      )
    )
  )
#Server---- 

  server <- function(input, output,session) {	
    
    output$data=shiny::renderUI({
      d<-names(obj)
      selData=d[1]
      shiny::selectInput("data","Select Data",d,selected = selData)
    })
    
    data.sel=shiny::eventReactive(input$data,{
      as.data.frame(obj[[input$data]])
    })  
    
    shiny::observeEvent(data.sel(),{
      output$annoVars<-shiny::renderUI({
        data.in=data.sel()
        NM=NULL
        
        if(any(sapply(data.in,class)=='factor')){
          NM=names(data.in)[which(sapply(data.in,class)=='factor')]  
        } 
        shiny::column(width=4,
                      shiny::selectizeInput('annoVar','Annotation',choices = names(data.in),selected=NM,multiple=T,options = list(placeholder = 'select columns',plugins = list("remove_button")))
        )
      })
      
      #Sampling UI ----  
      output$sample<-shiny::renderUI({
        list(
          shiny::column(4,shiny::textInput(inputId = 'setSeed',label = 'Seed',value = sample(1:10000,1))),
          shiny::column(4,shiny::numericInput(inputId = 'selRows',label = 'Number of Rows',min=1,max=pmin(500,nrow(data.sel())),value = pmin(500,nrow(data.sel())))),
          shiny::column(4,shiny::selectizeInput('selCols','Columns Subset',choices = names(data.sel()),multiple=T))
        )
      })
    })
    
    output$colUI<-shiny::renderUI({
      colSel=ifelse(input$transform_fun=='cor','RdBu','Vidiris')
      shiny::selectizeInput(inputId ="pal", label ="Select Color Palette",
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
    
    shiny::observeEvent({data.sel()},{
      output$colRng=shiny::renderUI({
        
        rng=range(data.sel(),na.rm = TRUE)
        
        n_data = nrow(data.sel())
        
        min_min_range = ifelse(input$transform_fun=='cor',-1,-Inf)
        min_max_range = ifelse(input$transform_fun=='cor',1,rng[1])
        min_value = ifelse(input$transform_fun=='cor',-1,rng[1])
        
        max_min_range = ifelse(input$transform_fun=='cor',-1,rng[2])
        max_max_range = ifelse(input$transform_fun=='cor',1,Inf)
        max_value = ifelse(input$transform_fun=='cor',1,rng[2])
        
        a_good_step = 0.1 # (max_range-min_range) / n_data
        
        list(
          shiny::numericInput("colorRng_min", "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
          shiny::numericInput("colorRng_max", "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
        )
        
      })  
    })
    
    
    interactiveHeatmap<- shiny::reactive({
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
      
      if(length(input$annoVar)>0){
        if(all(input$annoVar%in%names(data.in))) data.in=data.in%>%mutate_each_(funs(factor),input$annoVar)
      } 
      
      ss_num =  sapply(data.in, is.numeric) # in order to only transform the numeric values
      
      if(input$transpose) data.in=t(data.in)
      if(input$transform_fun!='.'){
        if(input$transform_fun=='is.na10') data.in=heatmaply::is.na10(data.in)
        if(input$transform_fun=='cor'){
          shiny::updateCheckboxInput(session = session,inputId = 'showColor',value = T)
          shiny::updateCheckboxInput(session = session,inputId = 'colRngAuto',value = F)
          data.in=stats::cor(data.in[, ss_num],use = "pairwise.complete.obs")
        }
        if(input$transform_fun=='log') data.in[, ss_num]= apply(data.in[, ss_num],2,log)
        if(input$transform_fun=='sqrt') data.in[, ss_num]= apply(data.in[, ss_num],2,sqrt) 
        if(input$transform_fun=='normalize') data.in=heatmaply::normalize(data.in)
        if(input$transform_fun=='scale') data.in[, ss_num] = scale(data.in[, ss_num])
        if(input$transform_fun=='percentize') data.in=heatmaply::percentize(data.in)
      } 
      
      
      #if(!is.null(input$tables_true_search_columns)) 
      #  data.in=data.in[activeRows(input$tables_true_search_columns,data.in),]
      if(input$colRngAuto){
        ColLimits=NULL 
      }else{
        ColLimits=c(input$colorRng_min, input$colorRng_max)
      }
      
      distfun_row = function(x) stats::dist(x, method = input$distFun_row)
      distfun_col =  function(x) stats::dist(x, method = input$distFun_col)
      
      hclustfun_row = function(x) stats::hclust(x, method = input$hclustFun_row)
      hclustfun_col = function(x) stats::hclust(x, method = input$hclustFun_col)
      
      heatmaply::heatmaply(data.in,
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
        plotly::layout(margin = list(l = input$l, b = input$b))
      
    })
    
    shiny::observeEvent(data.sel(),{
      output$heatout <- plotly::renderPlotly({
          interactiveHeatmap()
      })
    })
    
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
    shiny::observeEvent({interactiveHeatmap()},{
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
      
      
      l=data.frame(Parameter=names(l),Value=do.call('rbind',l),row.names = NULL,stringsAsFactors = F)
      l[which(l$Value==''),2]='NULL'
      paramTbl=print(xtable::xtable(l),type = 'html',include.rownames=FALSE,print.results = F,html.table.attributes = c('border=0'))
      
      
      h$width='100%'
      h$height='800px'
      s<-htmltools::tags$div(style="position: relative; bottom: 5px;",
                  htmltools::HTML(paramTbl),
                  htmltools::tags$em('This heatmap visualization was created using',
                                     htmltools::tags$a(href="https://github.com/yonicd/shinyHeatmaply/",target="_blank",'shinyHeatmaply'),
                          Sys.time()
                  )
      )
      
      output$downloadData <- shiny::downloadHandler(
        filename = function() {
          paste("heatmaply-", gsub(' ','_',Sys.time()), ".html", sep="")
        },
        content = function(file) {
          libdir <- paste(tools::file_path_sans_ext(basename(file)),"_files", sep = "")
          
          htmltools::save_html(htmltools::browsable(htmltools::tagList(h,s)),file=file,libdir = libdir)
          # if (!htmlwidgets:::pandoc_available()) {
          if (!pandoc_available()) {
            stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
                 "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
          }
          
          # htmlwidgets:::pandoc_self_contained_html(file, file)
          pandoc_self_contained_html(file, file)
          unlink(libdir, recursive = TRUE)
        }
      )
    })
    
  }
  
shiny::runGadget(ui, server, viewer = viewer)

}