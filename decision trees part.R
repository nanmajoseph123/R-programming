#Problem 7. Use once again the Wisconsin Prognostic Breast Cancer Data "wpbc" from the library
#"TH.data"
#Prepare a data frame "WPBC" with the selected columns 1, 2, 7, 14, 15, 29
#and all the rows of wpbc.
#Change the variable "time" from int to num. 

library(TH.data)
WPBC<-wpbc[c(1,2,7,14,15,29)]
WPBC$time<-as.numeric(WPBC$time)
str(WPBC)
s <- 49:198
train <- WPBC[s,]
test <- WPBC[-s,]
summary(train) # N = 116 R = 34
library(party)
fit<-ctree(status~.,data=train)
fit
summary(test) # N = 35 R = 13
testPred<-predict(fit,newdata=test)
table(test$status,testPred)
summary(train) # N = 116 R = 34
pred <- predict(fit,data=train)
table(train$status,pred)
# or with
T<-table(train$status,pred)
T
sum(diag(T))/sum(T)


#***********************************************************************************************************

#2) Body Fat. Use the dataset 'bodyfat', given in package 'TH.data' to 
#design a regression type decision tree that predicts DEXfat depending on :
#age, waistcirc, hipcirc, elbowbreadth, kneebreadth.
#Use 50 cases to train and 21 cases to test the model.

data("bodyfat",package="TH.data") 
str(bodyfat)
set.seed(1234)
s<-sample(71,50)
train <- bodyfat[s,]
test <- bodyfat[-s,]
treefit <- ctree(DEXfat~age+waistcirc+hipcirc+elbowbreadth+kneebreadth,data=train)
plot(treefit)
tree_pred <- predict(treefit,test)
diff <-(tree_pred-test[,"DEXfat"])/mean(test[,"DEXfat"])
summary(diff)
# We now fit a linear regression model
lmfit <- lm(DEXfat~age+waistcirc+hipcirc+elbowbreadth+kneebreadth,data=train)
summary(lmfit)
lm_pred <- predict(lmfit,test)
diff <-(lm_pred-test[,"DEXfat"])/mean(test[,"DEXfat"])
summary(diff)

#**************************************************************************************************************

#1) Mushrooms. Use the dataset 'mushrooms.csv' for a decision tree that relates the type of mushrooms, 
#edible or poisonous, to relevant predictors.
#Use 80% of the observations to train the model and 20% of them to test the predicted type.


library(party)
mushrooms <- read.csv(file.choose(),header=TRUE)
head(mushrooms)
str(mushrooms)
nrow(mushrooms)
sum(complete.cases(mushrooms))  # No NA's in the dataset
table(mushrooms[,17])
mushrooms <- mushrooms[,-17]  # Delete columne veil_type
table(mushrooms$type,mushrooms$odor)
#'e' stands for edible class and 'p' stands for the poisonous 
# Odor values 'c', 'f', 'm', 'p', 's' and 'y' are clearly poisonous.
# Mushrooms having odor values 'a' and 'l' are edible.
# 3408 mushrooms of odor 'n' are edible but 120 with odor 'n' are 
# poisonous.
# data splicing
set.seed(12345)
s <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),
            replace = FALSE)
# training set
mushrooms_train <- mushrooms[s,]
# test set
mushrooms_test <- mushrooms[-s,]
# Building the classification tree with ctree()
partyTree<-ctree(type~.,data=mushrooms_train)
plot(partyTree)
# All mushrooms with Odor = {c,f,m,p,s,y} are poisonous.
testPred<-predict(partyTree,newdata=mushrooms_test)
table(actual=mushrooms_test$type,predicted=testPred)


#***********************************************************************************************************
#*
#

library(party)
wbcd <- read.csv(file.choose(),header=TRUE) #"wisc_bc_data.csv" 
head(wbcd)
str(wbcd)
wbcd<-wbcd[,-1]
sum(complete.cases(wbcd))  # No NA's in the dataset
set.seed(1234)
s <- sample(569,455)
# training set
bc_train <- wbcd[s,]
# test set
bc_test <- wbcd[-s,]
head(bc_test)
Tree<-ctree(diagnosis~.,data=bc_train)
plot(Tree)
# All cases with concave.points_worst > 0.142 and
# radius_worst > 16.25 are of type "M" (malignant).
testPred<-predict(Tree,newdata=bc_test)
table(actual=bc_test$diagnosis,predicted=testPred)
# We take the first row of the test data to build a new case with 
# 'concave.points_worst' > 0.142 and 'radius_worst' > 16.25
new<-bc_test[1,]
new$concave.points_worst <- 0.15
new$radius_worst <- 16.5
predict(Tree,newdata=new)




