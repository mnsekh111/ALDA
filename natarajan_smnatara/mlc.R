rm(list=ls(all=T))

#install.packages("dplyr")
library(class)
library(e1071)
library(ellipse)
library(ggplot2) 
library(RColorBrewer)
library(dplyr)
#setwd("~/natarajan_smnatara")


trainingdata <- read.table("img-train.txt",header = TRUE,sep = ",")
testdata <- read.table("img-test.txt",header = TRUE,sep = ",")

MeanMatrix <- aggregate(trainingdata[, 1:3], list(trainingdata$Class), mean)

splitClass <- split(trainingdata, as.factor(trainingdata$Class))

cv1 <- solve(cov(splitClass$`1`[,1:3]))
cv2 <- solve(cov(splitClass$`2`[,1:3]))
cv3 <- solve(cov(splitClass$`3`[,1:3]))
cv4 <- solve(cov(splitClass$`4`[,1:3]))
cv5 <- solve(cov(splitClass$`5`[,1:3]))

det1 <- det(cov(splitClass$`1`[,1:3]))
det2 <- det(cov(splitClass$`2`[,1:3]))
det3 <- det(cov(splitClass$`3`[,1:3]))
det4 <- det(cov(splitClass$`4`[,1:3]))
det5 <- det(cov(splitClass$`5`[,1:3]))

MeanMatrix1 <- t(MeanMatrix[1,2:4])
MeanMatrix2 <- t(MeanMatrix[2,2:4])
MeanMatrix3 <- t(MeanMatrix[3,2:4])
MeanMatrix4 <- t(MeanMatrix[4,2:4])
MeanMatrix5 <- t(MeanMatrix[5,2:4])

prob1 <- nrow(splitClass$`1`)/nrow(trainingdata)
prob2 <- nrow(splitClass$`2`)/nrow(trainingdata)
prob3 <- nrow(splitClass$`3`)/nrow(trainingdata)
prob4 <- nrow(splitClass$`4`)/nrow(trainingdata)
prob5 <- nrow(splitClass$`5`)/nrow(trainingdata)

mlcmodel<- array(0,nrow(testdata))

for(i in 1:nrow(testdata))
{
  x<- t(testdata[i,1:3])
  a = (-0.5*t(x-MeanMatrix1) %*% cv1 %*%(x-MeanMatrix1)) + log(prob1) - 1.5*log(2*pi) - 0.5*log(det1)
  b = (-0.5*t(x-MeanMatrix2) %*% cv2 %*%(x-MeanMatrix2)) + log(prob2) - 1.5*log(2*pi) - 0.5*log(det2)
  c = (-0.5*t(x-MeanMatrix3) %*% cv3 %*%(x-MeanMatrix3)) + log(prob3) - 1.5*log(2*pi) - 0.5*log(det3)
  d = (-0.5*t(x-MeanMatrix4) %*% cv4 %*%(x-MeanMatrix4)) + log(prob4) - 1.5*log(2*pi) - 0.5*log(det4)
  e = (-0.5*t(x-MeanMatrix5) %*% cv5 %*%(x-MeanMatrix5)) + log(prob5) - 1.5*log(2*pi) - 0.5*log(det5)
  f <- c(a,b,c,d,e)
  mlcmodel[i]  = which.max(f)
}
actual <- testdata$Class
cm <- table(mlcmodel,actual)

overallacc = sum(diag(cm ))/sum(cm ) * 100


class1acc = cm[1,1]/(cm[1,1]+cm[2,1]+cm[3,1]+cm[4,1]+cm[5,1]) * 100

class2acc = cm[2,2]/(cm[1,2]+cm[2,2]+cm[3,2]+cm[4,2]+cm[5,2]) * 100

class3acc = cm[3,3]/(cm[1,3]+cm[2,3]+cm[3,3]+cm[4,3]+cm[5,3]) * 100

class4acc = cm[4,4]/(cm[1,4]+cm[2,4]+cm[3,4]+cm[4,4]+cm[5,4]) * 100

class5acc = cm[5,5]/(cm[1,5]+cm[2,5]+cm[3,5]+cm[4,5]+cm[5,5]) * 100


print("Confusion Matrix")
print(cm)
cat("Class 1 Accuracy",class1acc,"\n")
cat("Class 2 Accuracy",class2acc,"\n")
cat("Class 3 Accuracy",class3acc,"\n")
cat("Class 4 Accuracy",class4acc,"\n")
cat("Class 5 Accuracy",class5acc,"\n")
cat("Overall Accuracy",overallacc,"\n")

actualNbModel <- naiveBayes(as.factor(trainingdata$Class) ~ ., data=trainingdata)
naivePredicted <- testdata$Class
target <- predict(actualNbModel, testdata )
cm <- table(target, naivePredicted)
overallacc = sum(diag(cm))/sum(cm) * 100


class1acc = cm[1,1]/(cm[1,1]+cm[2,1]+cm[3,1]+cm[4,1]+cm[5,1]) * 100

class2acc = cm[2,2]/(cm[1,2]+cm[2,2]+cm[3,2]+cm[4,2]+cm[5,2]) * 100

class3acc = cm[3,3]/(cm[1,3]+cm[2,3]+cm[3,3]+cm[4,3]+cm[5,3]) * 100

class4acc = cm[4,4]/(cm[1,4]+cm[2,4]+cm[3,4]+cm[4,4]+cm[5,4]) * 100

class5acc = cm[5,5]/(cm[1,5]+cm[2,5]+cm[3,5]+cm[4,5]+cm[5,5]) * 100

print("Confusion Matrix")
print(cm)

cat("Class 1 Accuracy",class1acc,"\n")
cat("Class 2 Accuracy",class2acc,"\n")
cat("Class 3 Accuracy",class3acc,"\n")
cat("Class 4 Accuracy",class4acc,"\n")
cat("Class 5 Accuracy",class5acc,"\n")
cat("Overall Accuracy",overallacc,"\n")


trainingdata$Class <- as.factor(trainingdata$Class)
testdata$Class <- as.factor(testdata$Class)

colors <- brewer.pal(10, "Paired")

temp1 <- trainingdata$R
temp2 <- trainingdata$G
group <- trainingdata$Class
dataFrame <- data.frame(x=temp1, y=temp2, group=factor(group))

dataFrame_inter <- data.frame() 

for(g in levels(dataFrame$group)){dataFrame_inter <- rbind(dataFrame_inter, cbind(as.data.frame(with(dataFrame[dataFrame$group==g,], ellipse(cor(x, y),scale=c(sd(x),sd(y)),centre=c(mean(x),mean(y))))),group=g))} 
centroids <- aggregate(cbind(x,y)~group,dataFrame,mean)

f_plot = ggplot(data=dataFrame, aes(x=x, y=y,colour=group)) + geom_point(size=1.5, alpha=.6)+ geom_point(data = centroids, size = 4)  + geom_path(data=dataFrame_inter, aes(x=x, y=y,colour=group), size=1, linetype=2)
print(f_plot)
