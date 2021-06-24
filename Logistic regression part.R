#Problem 4. Wisconsin Prognostic Breast Cancer Data. Each record represents follow-up data for one 
#breast cancer case. These are consecutive patients seen by Dr. Wolberg since 1984, and include only 
#those cases exhibiting invasive breast cancer and no evidence of distant metastases at the time of diagnosis.
#data("wpbc", package = "TH.data")
#Use "mean_radius", " SE_texture" for a logistic regression model to predict the probability of 
#"status": a factor with levels N (nonrecur) and R (recur)


data("wpbc", package = "TH.data")
head(wpbc)
str(wpbc)
logreg <- glm(status~mean_radius+SE_texture,data=wpbc,binomial)
summary(logreg)
predict(logreg,list(mean_radius=17,SE_texture=1),"response")


#*******************************************************************************************************************


#MULTCOMP PROBLEM

library(multcomp)
plot(sbp~age,data=sbp,pch=16)
fit <- lm(sbp~age,data=sbp)
abline(fit)
plot(sbp~age,data=sbp,pch=16,col=gender)
fitm <- lm(sbp~age,subset(sbp,gender=="male"))
fitf <- lm(sbp~age,subset(sbp,gender=="female"))
abline(fitm,col=1)
abline(fitf,col=2)
fit
fitm
fitf
fitboth <- lm(sbp~age+gender,data=sbp)
fitboth
summary(fitboth)
fitboth2 <- lm(sbp~age+gender+age:gender,data=sbp)
summary(fitboth2)

#***************************************************************************************************************


#1) Kyphosis. Kyphosis refers to a forward flexion of the spine following corrective spinal surgery. A 
#study carried out to determine risk factors for kyphosis reported the data frame "kyphosis", given in
#the library "rpart". The data frame has 81 rows and 4 columns, representing data on children who have 
#had corrective spinal surgery.
#Kyphosis: a factor with levels 'absent' and 'present' indicating if a kyphosis (a type of deformation) 
#was present after the operation.
#Age: in months
#Number: the number of vertebrae involved
#Start: the number of the first (topmost) vertebra operated on.
#Fit three separate models of logistic regression, use the outputs to decide whether Age, Number or 
#Start appear to have a significant impact on the presence of kyphosis.
#Predict for Age = 100, Number = 4, and Start = 10 the probabilities for the presence of kyphosis

library(rpart)
fit<-glm(Kyphosis~Age+Number+Start,family=binomial,data=kyphosis)
summary(fit)
predict(fit,list(Age=100,Number=4,Start=10),type="response")

#2) Admission. Use the admission.txt data for a logistic regression model to predict chances of 
#admission for students based on the Graduate Record Examinations score (gre), the Grade 
#Point Average (gpa) and the rank of schools. Divide the dataset into 80% training and 20% 
# testing data. Evaluate the model by p-values. Predict probabilities for admission in the 
#training and test datasets.
#(Note: "rank" must be a factor!)

mydata<-read.delim(file.choose(),header=T) # data admission
str(mydata)
mydata$admit<-as.factor(mydata$admit)
mydata$rank<-as.factor(mydata$rank)
str(mydata)
set.seed(1234)
s<-sample(2,nrow(mydata),replace=T,prob=c(0.8,0.2))
train<-mydata[s==1,]
test<-mydata[s==2,]
# Logistic regression model
mymodel<-glm(admit~gre+gpa+rank,data=train,family="binomial")
summary(mymodel)
p1 <- predict(mymodel,train,type="response")
head(p1) # No. 5 was a test case
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted = pred1,Actual = train$admit)
tab1 # confusion matrix
1 - sum(diag(tab1))/sum(tab1) 
p2 <- predict(mymodel,test,type="response")
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted = pred2,Actual = test$admit)
tab2
1 - sum(diag(tab2))/sum(tab2)


#*****************************************************************************************************************


#3) Diabetes. Diabetes patient records are given in dataset 'diabetes.txt'. With
#NPG: Pregnancies, PGL: Glucose, DIA: Blood Pressure, TSF: Skin Thickness, INS: Insulin, 
#BMI: Body Mass Index, DPF: Diabetes Pedigree Function, AGE, and the outcome Diabetes.
#Use the first 600 rows of the dataset as training data and the remaining rows as test data.
#Develop a logistic regression model that uses NPG, PGL, BMI and DPF as predictors. 
#Predict probabilities for Diabetes in the training and test datasets

mydata <- read.delim(file.choose(),header=T) # data= "Diabetes.txt"
str(mydata)
mydata <- mydata[,-1]
mydata$Diabet <- as.factor(mydata$Diabet)
str(mydata)
head(mydata)
train<-mydata[c(1:600),]
test<-mydata[c(601:768),]
str(train)
str(test)
mymodel <- glm(Diabet~NPG+PGL+BMI+DPF,data=train,binomial)
summary(mymodel)
Trainpred <- predict(mymodel,train,type="response")pred1 <- ifelse(Trainpred>0.5,1,0)
tab1 <- table(Predicted = pred1,Actual = train$Diabet)
tab1
1 - sum(diag(tab1))/sum(tab1) # misclassification about 23% 
Testpred <- predict(mymodel,test,type="response")
pred2 <- ifelse(Testpred>0.5,1,0)
tab2 <- table(Predicted = pred2,Actual = test$Diabet)
tab2
1 - sum(diag(tab2))/sum(tab2) # misclassification in test data about 21%
