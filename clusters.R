library(cluster)
library(fpc)
library(clusterSim)
library(clusterCrit)


ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

data <- read.csv("datasets/glass.csv")
data
# Kmeans cluster analysis
kmeans <- kmeans(data[, 1:10], centers=6)
#kmeans = pam(data[, -1], centers=5)

plot(data[, c(4, 5)], col = kmeans$cluster + 1, lty = "solid", pch = 19, cex=0)
text(data[, c(4, 5)], labels=as.numeric(data[, 9]), cex=1, col=kmeans$cluster + 1)

intCriteria(as.matrix(sapply(data[, 1:7], as.numeric)), kmeans$cluster, c("Dunn", "Davies_Bouldin", "Silhouette"))

ClusterPurity(kmeans$cluster, data[, c("class")])

#data <- read.csv("seeds.data")
# Kmeans clustre analysis
#clus <- kmeans(data[, -8], centers=3)
#clus = pam(data[, -8], centers=3)

#plot(data[, c(2, 3)], col = clus$cluster + 1, lty = "solid", pch = 19, cex=0)
#text(data[, c(2, 3)], labels=as.numeric(data[, 8]), cex=1, col=clus$cluster + 1)