ui <- shinyUI(
  fluidPage(
    tags$head(tags$link(rel="stylesheet", href="styles.css", type="text/css"),tags$script(src="getdata.js")),
  sidebarLayout(
  sidebarPanel(
    h2(id="data-title", "Drop Datasets"),
    div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)",ondrop="dropData(event)"),
    uiOutput('data'),
    checkboxInput('showSample','Subset Data'),
    conditionalPanel('input.showSample',uiOutput('sample')),
    # br(),
    hr(),h4('Data Preprocessing'),
    column(width=4,selectizeInput('transpose','Transpose',choices = c('No'=FALSE,'Yes'=TRUE),selected = FALSE)),
    column(width=4,selectizeInput("transform_fun", "Transform", c(Identity=".",Sqrt='sqrt',log='log',Scale='scale',Normalize='normalize',Percentize='percentize',"Missing values"='is.na10', Correlation='cor'),selected = '.')),
    uiOutput('annoVars'),
    
    br(),hr(),h4('Row dendrogram'),
    column(width=4,selectizeInput("distFun_row", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
    column(width=4,selectizeInput("hclustFun_row", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
    # sliderInput("r", "Number of Clusters in Row", min = 1, max = 10, value = 2),    
    column(width=4,numericInput("r", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),   

    br(),hr(),h4('Column dendrogram'),
    column(width=4,selectizeInput("distFun_col", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
    column(width=4,selectizeInput("hclustFun_col", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
    # sliderInput("c", "Number of Clusters in Column", min = 1, max = 10, value = 2),
    column(width=4,numericInput("c", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),    
    
    br(),hr(),  h4('Additional Parameters'),
    
    column(3,checkboxInput('showColor','Color')),
    column(3,checkboxInput('showMargin','Layout')),
    column(3,checkboxInput('showDendo','Dendrogram')),
    hr(),
    conditionalPanel('input.showColor==1',
                     hr(),
    h4('Color Manipulation'),
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
                   selected='viridis'),
    sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
    checkboxInput('colRngAuto','Auto Color Range',value = T),
    conditionalPanel('!input.colRngAuto',uiOutput('colRng'))
    ),
    
    conditionalPanel('input.showDendo==1',
             hr(),
             h4('Dendrogram Manipulation'),
             selectInput('dendrogram','Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both'),
             selectizeInput("seriation", "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
             sliderInput('branches_lwd','Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1)
                     ),             

    conditionalPanel('input.showMargin==1',
    hr(),
    h4('Widget Layout'),
    column(4,textInput('main','Title','')),
    column(4,textInput('xlab','X Title','')),
    column(4,textInput('ylab','Y Title','')),
    sliderInput('row_text_angle','Row Text Angle',value = 0,min=0,max=180),
    sliderInput('column_text_angle','Column Text Angle',value = 45,min=0,max=180),
    sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
    sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40)
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Heatmaply",
               tags$a(id = 'downloadData', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, icon("clone"), 'Download Heatmap as HTML'),
               tags$head(tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
               plotlyOutput("heatout",height='600px')),
      tabPanel("Data",dataTableOutput('tables'))
    ) 
  )
)
)
)
