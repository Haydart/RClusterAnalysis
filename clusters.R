library(cluster)
library(fpc)
library(clusterSim)
library(clusterCrit)
library(colorspace)

MIN_CLUSTER_COUNT = 2
MAX_CLUSTER_COUNT = 10

clusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

plotDataPairs <- function(class_column) {
  pairs(data, col=class_column, lower.panel=NULL, cex.labels=2, pch=19, cex=1.2)
}

data <- read.csv("datasets/wine.csv")
labels <- data[, c("class")]
data <- data[, -which(names(data) %in% c("class"))]
data <- data.Normalization(data, type="n1", normalization="column")

real_col <- rev(rainbow_hcl(clusters_count))[as.numeric(labels)]
plotDataPairs(real_col)

for(clusters_count in MIN_CLUSTER_COUNT:MAX_CLUSTER_COUNT) {
  cat("\n\nClusters count", clusters_count, "\n")
  clusters <- kmeans(data, centers=clusters_count)
  
  cluster_col <- rev(rainbow_hcl(clusters_count))[as.numeric(clusters$cluster)]
  plotDataPairs(cluster_col)
  
  print(intCriteria(as.matrix(sapply(data, as.numeric)), clusters$cluster, c("Dunn", "Davies_Bouldin", "Silhouette")))
  print(clusterPurity(clusters$cluster, labels))
}