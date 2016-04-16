#Remove all local variables
rm(list=ls(all=T))

#setwd("~/natarajan_smnatara")
#Set the working directory where the source data files are places

# Library needed
library("e1071")

# Importing the training data
dtr3<- read.csv("dtr3.csv", header=FALSE)
dtr8<- read.csv("dtr8.csv", header=FALSE)
dtr<-rbind(dtr3,dtr8)

dtr$V1 <- factor(dtr$V1)


dte3 <- read.csv("dte3.csv", header=FALSE)
dte8 <- read.csv("dte8.csv", header=FALSE)
dte <- rbind(dte3,dte8)

dte$V1 <- factor(dte$V1) 

target<- dte[,1]

#dte_no_class <- subset( dte, select = -1 )
#dtr_no_class <- subset(dtr,select = -1)

#model
#model <- naiveBayes(dtr$V1~., data =dtr_no_class )
model <- naiveBayes(dtr$V1~., data =dtr )


#nb.prediction <- predict(nb.model,dte_no_class )
prediction <- predict(model,dte )


cm <- table(target,prediction)



print(cm)

acc3 <- cm [1,1]/(cm [1,1] + cm [1,2])*100
acc8 <- cm [2,2]/(cm [2,1] + cm [2,2])*100
acc <- (cm [2,2]+cm [1,1])/(cm [1,1]+ cm [1,2]+ cm [2,1] + cm [2,2])*100

cat("Class 3 accuracy  :",acc3,"\n")
cat("Class 8 accuracy  :",acc8,"\n")
cat("Overall accuracy  :",acc,"\n")

