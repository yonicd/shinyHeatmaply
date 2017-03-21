# shinyHeatmaply

Shiny application and Shiny gadget for the [heatmaply](https://github.com/talgalili/heatmaply) pacakge. Functionality of the heatmaply package is accessed through Shiny UI. 

Main difference between the Shiny application and the Shiny gadget:

  - The application has a import drag and drop interface as part of the application. Currently only csv files are supported.

  - The gadget is called from the R console and accepts input arguments. The object defined as the input to the shinyHeatmaply gadget is a data.frame or a list of data.frames.

Installation:

```r

install.packages("shiny") 
install.packages("htmlwidgets")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")    
install.packages("dendextend")    
install.packages("plotly")    
install.packages("devtools")    
# install.packages("heatmaply") # you'll need the latest version. The CRAN version is not updated yet.
devtools::install_github('talgalili/heatmaply')
install.packages("DT")    

```
Run Application:

```
library(shiny)    
runGitHub("yonicd/shinyHeatmaply")
```


Run Gadget:

```
library(heatmaply)
library(shiny)

data(mtcars)
launch_heatmaply()
```


