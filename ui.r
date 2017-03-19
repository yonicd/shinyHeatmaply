ui <- shinyUI(
  fluidPage(
    tags$head(tags$link(rel="stylesheet", href="styles.css", type="text/css"),
              tags$script(src="getdata.js")),
  sidebarLayout(
  sidebarPanel(
    h2(id="data-title", "Drop Datasets"),
    div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)",
        ondrop="dropData(event)"),
    uiOutput('data'),
    hr(),h3('Data Manipulation'),
    selectizeInput("f", "Transform Data", c(Identity=".", Correlation='cor',Sparse='na_mat'),selected = '.'),
    selectizeInput("seration", "Select Seration", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
    hr(),h3('Color Manipulation'),
    selectizeInput(inputId ="pal", label ="Select Color Palette",choices = c(Vidiris="viridis",BrBg="BrBG",Spectral="Spectral",Heat='heat.colors',Grey='grey.colors'),selected='viridis'),
    sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
    checkboxInput('colRngAuto','Auto Color Range',value = T),
    conditionalPanel('!input.colRngAuto',uiOutput('colRng')),
    hr(),h3('Widget Manipulation'),
    sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
    sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40),
    sliderInput("r", "Set Rows", min = 1, max = 11, value = 2),
    sliderInput("c", "Set Columns", min = 1, max = 11, value = 2)
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
