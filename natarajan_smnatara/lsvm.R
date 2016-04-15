
rm(list=ls(all=T))

#setwd("~/natarajan_smnatara")

library("e1071")


dtr3 <- read.csv("dtr3.csv", header=FALSE)
dtr8 <- read.csv("dtr8.csv", header=FALSE)
dtr<-rbind(dtr3,dtr8)


dte3 <- read.csv("dte3.csv", header=FALSE)
dte8 <- read.csv("dte8.csv", header=FALSE)
dte <- rbind(dte3,dte8)


testTarget<- dte[,1]

model <- svm(as.factor(V1)~., kernel = "linear", data = dtr)
prediction.model <- predict(model, dte)
cm <- table(testTarget,prediction.model)

print(cm)

acc3 <- cm [1,1]/(cm [1,1] + cm [1,2])*100
acc8 <- cm [2,2]/(cm [2,1] + cm [2,2])*100
acc <- (cm [2,2]+cm [1,1])/(cm [1,1]+ cm [1,2]+ cm [2,1] + cm [2,2])*100

cat("Class 3 accuracy  :",acc3,"\n")
cat("Class 8 accuracy  :",acc8,"\n")
cat("Overall accuracy  :",acc,"\n")




