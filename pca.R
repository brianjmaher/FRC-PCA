library(readr)
library(ggplot2)

data <- read_csv("2016IriCompOpr.csv")

dataAsMatrix <- as.matrix(data)[,2:ncol(data)]
teamNumbers <- as.matrix(data)[,1]
pca <- prcomp(dataAsMatrix, retx=TRUE, center=TRUE, scale.=TRUE)

plot <- ggplot(data=as.data.frame(pca$x), aes(x=PC1, y=PC2)) 
plot <- plot + geom_point()
plot
plot <- plot + geom_text(aes(label=teamNumbers),hjust=0, vjust=0)
plot
