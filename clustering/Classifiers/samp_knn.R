A1=c(0,0)
A2=c(1,1)
A3=c(2,2)

# Class B cases
B1=c(6,6)
B2=c(5.5,7)
B3=c(6.5,5)

# Build the classification matrix
train=rbind(A1,A2,A3, B1,B2,B3)

train 
# Class labels vector (attached to each class instance)
cl=factor(c(rep("A",3),rep("B",3)))
cl
# The object to be classified
test=c(4, 4)

test


# Load the class package that holds the knn() function
library(class)

# call knn() and get its summary
summary(knn(train, test, cl, k = 1))

# End of listing
