library(gplots)
options(max.print=1000000)
# setwd("N:/temp")
dd <- read.delim("C:\\Users\\junior\\Downloads\\rpkm-50.txt",sep="\t",header=TRUE,dec=".",stringsAsFactors = FALSE,strip.white = TRUE)
x <- as.matrix(dd)
rc <- rainbow(nrow(x), start=0, end=.3)
cc <- rainbow(ncol(x), start=0, end=.3)

# pdf(file='heatmap-spearman.pdf')
hr <- hclust(as.dist(1-cor(t(x), method="spearman")), method="complete")# spearman clustering
hc <- hclust(as.dist(1-cor(x, method="spearman")), method="complete")# spearman clustering
heatmap.2(x, col=bluered(75), Colv=as.dendrogram(hc), Rowv=as.dendrogram(hr), scale="row", key=T, keysize=1.5,density.info="none", trace="none",cexCol=0.9, cexRow=0.9,labRow=NA, dendrogram="both") # Z scores
# dev.off()

# dim(x)
# head(x)

library(heatmaply)
heatmaply(x, color=bluered(75), 
          Colv=as.dendrogram(hc), Rowv=as.dendrogram(hr), 
          k_col = 2, k_row = 2,
          scale="row", row_dend_left = TRUE,
          # key=T, keysize=1.5,density.info="none", trace="none",cexCol=0.9, cexRow=0.9,labRow=NA, 
          dendrogram="both") # Z scores
