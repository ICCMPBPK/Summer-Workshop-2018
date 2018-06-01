
install.packages("gplots")

setwd("C:/Users/wucen/Desktop/Workshop/Network")

library("gplots")

correl = cor(G)
my_palette <- colorRampPalette(c("green", "yellow", "red"))(n = 299)

# png("./heatmap_skcm.png", width = 5*300, height = 5*300, res = 300, pointsize = 8)
col_breaks = c(seq(-1,-0.4,length=100),
               seq(-0.39,0.4,length=100),
               seq(0.41,1,length=100))
heatmap.2(correl, dendrogram='none', Rowv=TRUE, Colv=TRUE, trace='none', labRow="", labCol="",
          col=my_palette, breaks=col_breaks)
# dev.off()