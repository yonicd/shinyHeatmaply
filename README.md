# shinyHeatmaply

Shiny application and Shiny gadget for the [heatmaply](https://github.com/talgalili/heatmaply) pacakge. Functionality of the heatmaply package is accessed through Shiny UI. 

Main difference between the Shiny application and the Shiny gadget:

  - The application has an import interface as part of the application.
    - Currently: csv,txt,tab,xls,xlsx,rd,rda files are supported.

  - The gadget is called from the R console and accepts input arguments. The object defined as the input to the shinyHeatmaply gadget is a data.frame or a list of data.frames.

Installation:

```r
devtools::install_github('yonicd/shinyHeatmaply')
```

Run Application:

```
runGitHub("yonicd/shinyHeatmaply",subdir = 'inst/shinyapp')
```


Run Gadget:

```
library(shinyHeatmaply)

#single data.frame
data(mtcars)
launch_heatmaply(mtcars)

#list
data(iris)
launch_heatmaply(list('Example1'=mtcars,'Example2'=iris))
```

