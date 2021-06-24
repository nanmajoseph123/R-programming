#Problem 5. Use the Wisconsin Prognostic Breast Cancer Data "wpbc" from the library "TH.data"
#Prepare a data frame "WPBC" with the selected columns 1, 2, 7, 14, 15, 29
#and all the rows of wpbc.
#Change the variable "time" from int to num. 

library(TH.data)
WPBC<-wpbc[c(1,2,7,14,15,29)]
WPBC$time<-as.numeric(WPBC$time)
str(WPBC)
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}
WPBC_n <- as.data.frame(lapply(WPBC[2:6],normalize))
WPBC_n
library(class)
s <- 1:150
train <- WPBC_n[s,]
test <- WPBC_n[-s,]
train_labels <- WPBC[s,1]
test_labels <- WPBC[-s,1]
sqrt(150)
# => k = 13
pred <- knn(train=train, test=test, cl=train_labels, k=13)
table(actual=test_labels,predicted=pred)
test_labels <- WPBC[-s,1]
table(actual=test_labels,predicted=pred)
# accuracy:
(29+7)/(29+10+2+7)

#*******************************************************************************************************************


#Problem 6. Use the dataset ruspini from the R-package 'cluster' to find 3 clusters. Use nstart=25.
#A data frame with 75 observations on 2 variables giving the x and y coordinates of the points, 
#respectively.
data("ruspini", package = "cluster")
str(ruspini)
head(ruspini)
result <- kmeans(ruspini, 3,nstart=25)
result

#*****************************************************************************************************************

