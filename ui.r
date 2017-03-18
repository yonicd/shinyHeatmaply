ui <- shinyUI(
  fluidPage(
    tags$head(tags$link(rel="stylesheet", href="styles.css", type="text/css"),
              tags$script(src="getdata.js")),
  sidebarLayout(
  sidebarPanel(
    h3(id="data-title", "Drop Datasets"),
    div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)",
        ondrop="dropData(event)"),
    uiOutput('data'),
    downloadLink("downloadData", "Download Widget"),
    radioButtons("f", "Transform Data", c(Identity=".", Correlation='cor',Sparse='na_mat'),inline = T,selected = '.'),
    radioButtons("seration", "Select Seration", c(OLO="OLO",GW="GW",Mean="mean",None="none"),inline = T,selected = 'OLO'),
    radioButtons("pal", "Select Color Palette", c(Vidiris="viridis",BrBg="BrBG",Spectral="Spectral",Heat='heat.colors',Grey='grey.colors'),inline = T,selected = 'viridis'),
    sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
    checkboxInput('colRngAuto','Auto Color Range',value = T),
    conditionalPanel('!input.colRngAuto',uiOutput('colRng')),
    sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
    sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40),
    sliderInput("r", "Set Rows", min = 1, max = 11, value = 2),
    sliderInput("c", "Set Columns", min = 1, max = 11, value = 2)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Heatmaply",
               plotlyOutput("heatout",height='600px')),
      tabPanel("Data",dataTableOutput('tables'))
    ) 
  )
)
)
)