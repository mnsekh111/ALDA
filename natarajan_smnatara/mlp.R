rm(list=ls(all=T))

#setwd("~/natarajan_smnatara")

#install.packages("neuralnet")
#install.packages("ade4")
library(class)
library(neuralnet)
library(e1071)
library(ade4)

dtr3<- read.csv("dtr3.csv", header=FALSE)
dtr8<- read.csv("dtr8.csv", header=FALSE)
dtr <- rbind(dtr3, dtr8)

dte3 <- read.csv("dte3.csv", header=FALSE)
dte8 <- read.csv("dte8.csv", header=FALSE)
dte <- rbind(dte3 ,dte8)

set.seed(128)

colnames(dtr) <- paste0("V",1:ncol(dtr))
colnames(dte) <- paste0("V",1:ncol(dte))

dtr <- cbind(dtr, acm.disjonctif(dtr['V1']))
dtr$V1 <- NULL

alln <- names(dtr)

allnPredict <- alln[1:(length(alln)-2)]
temp <- paste(allnPredict, collapse = " + ")
temp1 <- alln[(length(alln)-2+1):length(alln)]
temp2 <- paste(temp1, collapse = " + ")
nn <- neuralnet(paste(temp2, "~", temp), dtr, hidden=1, threshold = 0.5)

allLabels <- c(3,8)

Actualtest <- dte$V1

temp1 <- compute(nn, dte[,2:ncol(dte)])$net.result
target <- allLabels[apply(temp1, 1, which.max)]
cm <- table(target,Actualtest)

print(cm)
acc3 <- cm [1,1]/(cm [1,1] + cm [1,2])*100
acc8 <- cm [2,2]/(cm [2,1] + cm [2,2])*100
acc <- (cm [2,2]+cm [1,1])/(cm [1,1]+ cm [1,2]+ cm [2,1] + cm [2,2])*100

cat("Class 3 accuracy  :",acc3,"\n")
cat("Class 8 accuracy  :",acc8,"\n")
cat("Overall accuracy  :",acc,"\n")
