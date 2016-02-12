# Read the data file
dataset = read.csv("~/GitHub/ALDA/hw2-data.csv");

# View the dataset
# View(dataset);

# Refining the dataset
dataset.features = dataset;

# Removing unncessary attributes
dataset.features$Id <- NULL;

methods <- c("single","complete","average","centroid")

prox_max <- dist(dataset.features,method = "euclidean");
for (i in methods) {
  result = hclust(prox_max,method = i);
  # output file
  jpeg(file = paste("~/GitHub/ALDA/h_clust_",i,".jpeg",sep = ""));
  
  groups <- cutree(result,k=4);
  # Generate plot
  plot(result);
  
  rect.hclust(result,k=4,border = "green");
  
  dev.off();
}