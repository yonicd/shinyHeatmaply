

distfun_row =  "euclidean"
hclustfun_row = "complete"
distfun_col = "euclidean"
hclustfun_col = "complete"


# choose dist method for row:
distfun_row <- switch(distfun_row,
                      "euclidean" = function(x) dist(x, method = "euclidean"),
                      "maximum" = function(x) dist(x, method = "maximum"),
                      "manhattan" = function(x) dist(x, method = "manhattan"),
                      "canberra" = function(x) dist(x, method = "canberra"),
                      "binary" = function(x) dist(x, method = "binary"),
                      "minkowski" = function(x) dist(x, method = "minkowski")
)
# choose hclust method for row:
hclustfun_row <- switch(hclustfun_row,
                        "complete" = function(x) hclust(x, method = "complete"),
                        "single" = function(x) hclust(x, method = "single"),
                        "average" = function(x) hclust(x, method = "average"),
                        "mcquitty" = function(x) hclust(x, method = "mcquitty"),
                        "median" = function(x) hclust(x, method = "median"),
                        "centroid" = function(x) hclust(x, method = "centroid"),
                        "ward.D" = function(x) hclust(x, method = "ward.D"),
                        "ward.D2" = function(x) hclust(x, method = "ward.D2")
)


# choose dist method for col:
distfun_col <- switch(distfun_col,
                      "euclidean" = function(x) dist(x, method = "euclidean"),
                      "maximum" = function(x) dist(x, method = "maximum"),
                      "manhattan" = function(x) dist(x, method = "manhattan"),
                      "canberra" = function(x) dist(x, method = "canberra"),
                      "binary" = function(x) dist(x, method = "binary"),
                      "minkowski" = function(x) dist(x, method = "minkowski")
)

# choose hclust method for col:
hclustfun_col <- switch(hclustfun_col,
                        "complete" = function(x) hclust(x, method = "complete"),
                        "single" = function(x) hclust(x, method = "single"),
                        "average" = function(x) hclust(x, method = "average"),
                        "mcquitty" = function(x) hclust(x, method = "mcquitty"),
                        "median" = function(x) hclust(x, method = "median"),
                        "centroid" = function(x) hclust(x, method = "centroid"),
                        "ward.D" = function(x) hclust(x, method = "ward.D"),
                        "ward.D2" = function(x) hclust(x, method = "ward.D2")
)



heatmaply(mtcars, 
          distfun_row =  distfun_row,
          hclustfun_row = hclustfun_row,
          distfun_col = distfun_col,
          hclustfun_col = hclustfun_col,
          
          margins = c(40, 130))


