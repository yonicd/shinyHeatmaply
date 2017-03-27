#


#' @import heatmaply
NULL

#' @import dplyr
NULL


# when a function is renamed, its document in man must be removed - otherwise it may cause problems with the built check (it will try to run the code in the example, and will fail.)
# When all is done, run:
# library(devtools)
# check()
# browseURL(tempdir())
### http://www.rstudio.com/ide/docs/packages/build_options
#
# check(build_args="--no-build-vignettes --no-manual", args = "--no-examples --no-build-vignettes --no-manual",  cran = FALSE, cleanup = FALSE)
# check(build_args="--no-build-vignettes --no-manual", args = "--no-build-vignettes --no-manual",  cran = FALSE, cleanup = FALSE)
# check(build_args="--no-build-vignettes ", args = "--no-build-vignettes",  cran = FALSE, cleanup = FALSE)
# devtools::check(args="--as-cran")
# devtools::check("C:/Dropbox/aaaa good R code/AA - My packages/heatmaply", args="--as-cran")
#                 Thanks to: http://stackoverflow.com/questions/10017702/r-cmd-check-options-for-more-rigorous-testing-2-15-0


# shell('git log --graph --stat --date=short --pretty=format:"%ad(%an) %s |%h" > ChangeLog', intern = TRUE)
# file.copy("NEWS", "NEWS.md",overwrite = TRUE)
# devtools::build_win(version="R-release")
# devtools::build_win(version="R-devel")
# release()



