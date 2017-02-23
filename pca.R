library(readr)
library(ggplot2)

data <- read_csv("2016NyroCompOpr.csv")

dataAsMatrix <- as.matrix(data)[,2:ncol(data)]
teamNumbers <- as.matrix(data)[,1]
pca <- prcomp(dataAsMatrix, retx=TRUE, center=TRUE, scale.=TRUE)

k = 6
clust = kmeans(dataAsMatrix, k, nstart=10)

plot <- ggplot(data=as.data.frame(pca$x), aes(x=PC1, y=PC2, color=clust$cluster)) 
plot <- plot + geom_point()
plot <- plot + scale_color_gradientn(colors=rainbow(k))# + coord_fixed()
plot <- plot + geom_text(aes(label=teamNumbers),hjust=0, vjust=0)
plot <- plot + ggtitle("Finger Lakes Regional")
plot
