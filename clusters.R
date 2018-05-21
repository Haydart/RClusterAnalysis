library(cluster)
library(fpc)
library(clusterSim)
library(clusterCrit)
library(colorspace)


ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

data <- read.csv("datasets/wine.csv")
labels <- data[, c("class")]
data <- data[, -which(names(data) %in% c("class"))]
data <- data.Normalization(data, type="n1", normalization="column")
data
# Kmeans cluster analysis
clusters <- kmeans(data, centers=6)
#clusters = pam(data[, -1], centers=5)

#plot(data[, c(4, 5)], col = clusters$cluster + 1, lty = "solid", pch = 19, cex=0)
#text(data[, c(4, 5)], labels=as.numeric(data[, c("class")]), cex=1, col=clusters$cluster + 1)

species_col <- rev(rainbow_hcl(3))[as.numeric(labels)]

pairs(data, col=species_col,
      lower.panel=NULL,
      cex.labels=2, pch=19, cex=1.2)

par(xpd=TRUE)
legend(x=0.05, y = 0.4, cex=2,
       legend = as.character(levels(labels)),
       fill = unique(species_col))
par(xpd=NA)

intCriteria(as.matrix(sapply(data, as.numeric)), clusters$cluster, c("Dunn", "Davies_Bouldin", "Silhouette"))

ClusterPurity(clusters$cluster, labels)

#data <- read.csv("seeds.data")
# Kmeans clustre analysis
#clus <- kmeans(data[, -8], centers=3)
#clus = pam(data[, -8], centers=3)

#plot(data[, c(2, 3)], col = clus$cluster + 1, lty = "solid", pch = 19, cex=0)
#text(data[, c(2, 3)], labels=as.numeric(data[, 8]), cex=1, col=clus$cluster + 1)