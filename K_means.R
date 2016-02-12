#Read the data file
dataset = read.csv("~/GitHub/ALDA/hw2-data.csv");

#View the dataset
View(dataset);
dataset.features = dataset;
dataset.features$Id <- NULL;

for (i in 2:6) {
  result <- kmeans(dataset.features,i);
  jpeg(file = paste("~/GitHub/ALDA/k_means_",i,".jpeg",sep = ""));
  plot(dataset.features,col = result$cluster);
  dev.off();
}


