# Read the data file
dataset = read.csv("~/GitHub/ALDA/hw2-data.csv");

# View the dataset
View(dataset);

# Refining the dataset
dataset.features = dataset;

# Removing unncessary attributes
dataset.features$Id <- NULL;

result2 <- kmeans(dataset.features,2);
result3 <- kmeans(dataset.features,3);
result4 <- kmeans(dataset.features,4);
result5 <- kmeans(dataset.features,5);
result6 <- kmeans(dataset.features,6);

#Creating a display matrix to show the output
par(mfrow=c(2,3))

plot(main = "2 Clusters" , dataset.features,col = result2$cluster);
plot(main = "3 Clusters" ,dataset.features,col = result3$cluster);
plot(main = "4 Clusters" ,dataset.features,col = result4$cluster);
plot(main = "5 Clusters" ,dataset.features,col = result5$cluster);
plot(main = "6 Clusters" ,dataset.features,col = result6$cluster);

