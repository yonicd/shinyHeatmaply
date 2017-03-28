# shinyHeatmaply

Shiny application and Shiny gadget for the [heatmaply](https://github.com/talgalili/heatmaply) pacakge. Functionality of the heatmaply package is accessed through Shiny UI. 

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
install_packages('shinyHeatmaply')
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
runGitHub("yonicd/shinyHeatmaply",subdir = 'inst/shinyapp')
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
