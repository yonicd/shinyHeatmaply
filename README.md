[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/shinyHeatmaply)](https://cran.r-project.org/package=shinyHeatmaply)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/shinyHeatmaply)

# shinyHeatmaply

Shiny application and Shiny gadget for the [heatmaply](https://github.com/talgalili/heatmaply) pacakge. Functionality of the heatmaply package is accessed through Shiny UI. 

We introduce a functionality that [saves to disk](https://yonicd.github.io/shinyHeatmaply/) a self contained copy of the htmlwidget as an html file with your data and specifications you set from the UI, so it can be embedded in webpages, blogposts and online web appendices for academic publications.

### Video Introduction:

<a href="http://www.youtube.com/watch?v=hANY_g1kB_A" target="_blank" ><img src="http://img.youtube.com/vi/hANY_g1kB_A/0.jpg" alt="shinyHeatmaply"></a>

<!----
<div class="iframe_container">
  <iframe width="560" height="315" src="http://www.youtube.com/embed/hANY_g1kB_A" frameborder="0" allowfullscreen></iframe>
</div>
---->

### Install

  - CRAN:

```r
install.packages('shinyHeatmaply')
```

  - Dev:

```r
devtools::install_github('yonicd/shinyHeatmaply')
```

### Shiny App or Shiny Gadget?

  - The application has an import interface as part of the application.
    - Currently: csv,txt,tab,xls,xlsx,rd,rda files are supported.

  - The gadget is called from the R console and accepts input arguments. The object defined as the input to the shinyHeatmaply gadget is a data.frame or a list of data.frames.

### Launching

Application:


```r
library(shiny)
library(heatmaply)
# If you didn't get shinyHeatmaply yet, you can run it through github:
# runGitHub("yonicd/shinyHeatmaply",subdir = 'inst/shinyapp')
# or just use your locally installed package:
library(shinyHeatmaply)
runApp(system.file("shinyapp", package = "shinyHeatmaply"))
```


Gadget:

```r
library(shinyHeatmaply)

#single data.frame
data(mtcars)
launch_heatmaply(mtcars)

#list
data(iris)
launch_heatmaply(list('Example1'=mtcars,'Example2'=iris))
```

### Example of saved htmlwidget [Output](https://yonicd.github.io/shinyHeatmaply/)
<!----
<iframe width="854" height="480" src="https://yonicd.github.io/shinyHeatmaply/" frameborder="0" allowfullscreen></iframe>
---->
