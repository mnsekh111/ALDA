# Read the data file
dataset = read.csv("~/GitHub/ALDA/hw2-data.csv");

# View the dataset
View(dataset);

# Refining the dataset
dataset.features = dataset;

# Removing unncessary attributes
dataset.features$Id <- NULL;

for (i in 2:6) {
  result <- kmeans(dataset.features,i);
  
  # output file
  jpeg(file = paste("~/GitHub/ALDA/k_means_",i,".jpeg",sep = ""));
  
  # Generate plot
  plot(dataset.features,col = result$cluster);
  
  dev.off();
}


