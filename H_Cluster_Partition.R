dataset <- read.csv('~/GitHub/ALDA/hw2-data.csv')
length(dataset)
dataset.points <- dataset
dataset.points$Id <- NULL
dataset.dist <- dist(dataset.points)

#Each for a separate method
dataset.shclust <- hclust(dataset.dist, method = "single")
dataset.chclust <- hclust(dataset.dist, method = "complete")
dataset.ahclust <- hclust(dataset.dist, method = "average")
dataset.cthclust <- hclust(dataset.dist, method = "centroid")


plot(dataset.shclust, main="Single", hang=-1)
rect.hclust(dataset.shclust, k=4)
plot(dataset.chclust, main="Complete", hang=-1)
rect.hclust(dataset.chclust, k=4)
plot(dataset.ahclust, main="Average", hang=-1)
rect.hclust(dataset.ahclust, k=4)
plot(dataset.cthclust, main="Centroid", hang=-1)
rect.hclust(dataset.cthclust, k=4)

dataset.shclust.splitted <- cut(as.dendrogram(dataset.shclust), h=6.2)
plot(dataset.shclust.splitted$lower[[1]], main="Single - 1")
plot(dataset.shclust.splitted$lower[[2]], main="Single - 2")
plot(dataset.shclust.splitted$lower[[3]], main="Single - 3")
plot(dataset.shclust.splitted$lower[[4]], main="Single - 4")


dataset.chclust.splitted <- cut(as.dendrogram(dataset.chclust), h=42.5)
plot(dataset.chclust.splitted$lower[[1]], main="Complete - 1")
plot(dataset.chclust.splitted$lower[[2]], main="Complete - 2")
plot(dataset.chclust.splitted$lower[[3]], main="Complete - 3")
plot(dataset.chclust.splitted$lower[[4]], main="Complete - 4")


dataset.ahclust.splitted <- cut(as.dendrogram(dataset.ahclust), h=20)
plot(dataset.ahclust.splitted$lower[[1]], main="Average - 1")
plot(dataset.ahclust.splitted$lower[[2]], main="Average - 2")
plot(dataset.ahclust.splitted$lower[[3]], main="Average - 3")
plot(dataset.ahclust.splitted$lower[[4]], main="Average - 4")

dataset.cthclust.splitted <- cut(as.dendrogram(dataset.cthclust), h=13.3)
plot(dataset.cthclust.splitted$lower[[1]], main="Centroid - 1")
plot(dataset.cthclust.splitted$lower[[2]], main="Centroid - 2")
plot(dataset.cthclust.splitted$lower[[3]], main="Centroid - 3")
plot(dataset.cthclust.splitted$lower[[4]], main="Centroid - 4")