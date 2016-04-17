rm(list = ls(all=T))
library("psych")
library("e1071")
library("class")
library("caret")
library("rminer")
library("rpart")
library("rpart.plot")
library("plyr")
library("xlsx")

dtr3=read.csv("dtr3.csv",header=FALSE,sep=",")
dtr8=read.csv("dtr8.csv",header=FALSE,sep=",")
dte3=read.csv("dte3.csv",header=FALSE,sep=",")
dte8=read.csv("dte8.csv",header=FALSE,sep=",")
train=rbind(dtr3,dtr8)
test=rbind(dte3,dte8)
test.label=test[,1]

# http://stackoverflow.com/questions/9666212/how-to-compute-error-rate-from-a-decision-tree

require(tree)
treemodel=tree(V1~.,data=train)
# print treemodel
summary(treemodel)
plot(treemodel)

prediction<- predict(treemodel, test)
prediction=ifelse(prediction < 5, 3, 8)
cm=table(as.integer(prediction),test.label)

print("Confusion matrix is")
print(cm)
class3_acc=cm[1,1]/(cm[1,1]+cm[2,1])
class5_acc=cm[2,2]/(cm[1,2]+cm[2,2])
total_acc=(cm[1,1]+cm[2,2])/sum(cm)
cat("class 3 accuracy ",class3_acc*100,"\n")
cat("class 8 accuracy ",class5_acc*100,"\n")
cat("overall accuracy ",total_acc*100,"\n")

