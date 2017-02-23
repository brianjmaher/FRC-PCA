library(readr)
library(ggplot2)

data <- read_csv("2016NyroCompOpr.csv")

dataAsMatrix <- as.matrix(data)[,2:ncol(data)]
teamNumbers <- as.matrix(data)[,1]
pca <- prcomp(dataAsMatrix, retx=TRUE, center=TRUE, scale.=TRUE)

bestClust = 0
bestK = 0
bestRatio = 0
for (k in 2:10) {
  clust = kmeans(dataAsMatrix, 5, nstart=10)
  ratio = clust$betweenss / clust$totss
  if (ratio > bestRatio) {
    bestK = k
    bestClust = clust
    bestRatio = ratio
  }
}

plot <- ggplot(data=as.data.frame(pca$x), aes(x=PC1, y=PC2, color=bestClust$cluster)) 
plot <- plot + geom_point()
plot <- plot + scale_color_gradientn(colors=rainbow(bestK))# + coord_fixed()
plot <- plot + geom_text(aes(label=teamNumbers),hjust=0, vjust=0)
plot
