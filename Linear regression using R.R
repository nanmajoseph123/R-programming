#Regression Analysis
#Problem 1. The article "An Experimental Correlation of Oxides of Nitrogen Emissions from Power
#Boilers Based on Field Data" (J. of Engr. for Power, July 1973: 165-170) reports data
#with X = burnerarea liberation rate (MBtu/hr-ft²) and Y= NOxemission rate (ppm). (Data: ex12.19)
#a. Form a scatterplot of the data Y = f(X).
#b. Find the least squares regression line.
#c. What are the p-values, what is R²?
#d. What is the estimate of expected NOx emission rate when burner area liberation rate equals 225?
#e. Calculate the coefficient of correlation.

install.packages("Devore7")
library(Devore7)
ex12.19
plot(Y~X,data=ex12.19)
fit<-lm(Y~X,data=ex12.19)
fit
summary(fit)
abline(fit)
predict(fit,list(X=225))
cor.test(ex12.19$X,ex12.19$Y)

#**********ANSWERS TO BE WRIITEN FORMAT :
#regression function :
# b) Y = -45.55191 + 1.71143*X
# c)-valpues of the coefficients:
# p-value of intercept: 0.0989
# p-value of slope: 8.23e-10
# d)estimate of expected NOx emission rate when burner area liberation rate equals 225 : 339.5204
# c)Multiple R-squared: 0.9609
# e)coefficient of correlation :0.9802442

#**************************************************************************************************************

#Problem 2. Snowpacks contain a substantial amount of pollutants that may represent environmental
#hazards. The article "Atmospheric PAH Deposition: Deposition Velocities and Washout Ratios" (J. of
#Environmental Engineering, 2002: 186-195) focused on the deposition of polyaromatic
#hydrocarbons. The authors proposed a multiple regression model for relating deposition over a
#specified time period (y, in ??g/m²) to two predictors x1 (??g-sec/m3) time and x2 (??g/m2) amount of
#precipitation. (ex13.53)
#Is there any significant interaction between x1 and x2?

ex13.53
plot(filth~.,data=ex13.53)
fit1<-lm(filth~.,data=ex13.53)
fit1
summary(fit1)
fit2 <- lm(filth~x1+x2+x1:x2,data=ex13.53)
fit2
summary(fit2)

#***************************************************************************************************************
#Problem 3. High-alumina refractory castables have been extensively investigated in recent years
#because of their significant advantages over other refractory brick of the same class- lower
#production and application costs, versatility, and performance at high temperatures. The
#accompanying data (on x: viscosity and y: free-flow) was read from a graph in the article "Processing
#of Zero-Cement Self-Flow Alumina Castables" (The Amer. Ceramic Soc. Bull., 1998: 60-66).
#data: ex13.29
#Fit a quadratic regression function y = a + bx + cx²
#Coefficients:
#p-values:
#Adjusted R-squared:

ex13.29
plot(y~x,ex13.29)
fit3<-lm(y~x,ex13.29)
fit3
fit4<-lm(y~x+I(x^2),ex13.29)
fit4
summary(fit4)
predict(fit4,list(x=5))

#Regression function: y = 111.886 + 8.064 x - 1.839x²
#p-values: 0.00363 0.18176 0.06191
#Adjusted R-squared: 0.9214
#Predict y for x = 5

#****************************************************************************************************************

# mtcars problem
head(mtcars)
str(mtcars)
summary(mtcars)
plot(mtcars)
plot(mtcars,gap=0)
plot(mpg~wt,data=mtcars)
fit5 <- lm(mpg~wt,data=mtcars)
fit5
# mpg = 37.285 - 5.344*wt
abline(fit5,col="red")
summary(fit5)
with(mtcars,cor.test(mpg,wt))
predict(fit5,list(wt=3))
predict(fit5,list(wt=c(2.5,3,4)))

#*****************************************************************************************************************


#AIR QUALITY PROBLEM
head(airquality)
plot(airquality)
plot(Ozone~Temp,data=airquality)
fit6 <- lm(Ozone~Temp,data=airquality)
abline(fit6)
plot(Ozone~Temp,data=airquality)
fit7 <- lm(Ozone~Temp+I(Temp^2),data=airquality)
fit7
x<-60:95
y<-predict(fit7,list(Temp=x))
 lines(x,y,col="red")
 fit8 <- lm(Ozone~Temp+I(Temp^2)+I(Temp^3)+I(sin(Temp)),data=airquality)
 fit8
 summary(fit8)

#*****************************************************************************************************************
 
 
 #1) Soil Acidity. The data given in (ex13.32) expresses the relationship between x = soil pH and y = 
 #Al Concentration/EC ("Root Responses of Three Gramineae Species to Soil Acidity in an Oxisol and 
  # an Ultisol," Soil Science, 1973: 295-302).
 #Fit a linear, a quadratic, and a cubic model: y=f(x). Discuss the p-values.
 #Using the quadratic model, what value of y would you predict when soil pH is x = 5?
 
 ex13.32
 plot(Y~X,pch=16,data=ex13.32)
 fit1<-lm(Y~X,data=ex13.32)
 summary(fit1)
 abline(fit1)
 fit2<-lm(Y~X+I(X^2),data=ex13.32)
 summary(fit2)
 x<-sort(ex13.32$X)
 y<-predict(fit2,list(X=x))
 lines(x,y,col="blue")
 fit3<-lm(Y~X+I(X^2)+I(X^3),data=ex13.32)
 summary(fit3)
 y<-predict(fit3,list(X=x))
 lines(x,y,col="red")
 predict(fit2,list(X=5))
 
 
 #*************************************************************************************************************
 
 #Waste Incineration. Information about energy content of the waste is needed for an efficient 
 #design of municipal waste incinerators. The article "Modeling the Energy Content of Municipal Solid 
 #Waste Using Multiple Regression Analysis" (J. of the Air and Waste Mgmnt. Assoc., 1996: 650-656)
 #provides us with the accompanying data on energy content, the three physical composition variables % 
 #plastics by weight, % paper by weight, and % garbage by weight, and % moisture by weight for waste 
 #specimens obtained from a certain region. (ex13.47)
 #Fit a multiple regression model with the four mentioned input variables as predictors of energy 
 #content. Explain the p-values of the coefficients.
 #Predict Energy Content for Plastics=20, Paper=25, Garbage=30, Water=50
 
 ex13.47
 mydata <- ex13.47[,-1] 
 mydata
 fit<-lm(Energy.content~., data=mydata)
fit 
summary(fit)
predict(fit,list(Plastics=20,Paper=25,Garbage=30,Water=50))

#**************************************************************************************************************
#3) Environmental Hazard. Snowpacks contain a substantial amount of pollutants that may represent 
#environmental hazards. The article "Atmospheric PAH Deposition: Deposition Velocities and 
#Washout Ratios" (J. of Environmental Engineering, 2002: 186-195) focused on the deposition of 
#polyaromatic hydrocarbons. The authors proposed a multiple regression model for relating deposition 
#over a specified time period (y, in ??g/m²) to two predictors x1 (??g-sec/m3
#) time and x2 (??g/m2)amount of precipitation. (ex13.53)
ex13.53
plot(filth~x1+x2,data=ex13.53)
fit9 <- lm(filth~x1+x2,data=ex13.53)
fit9
summary(fit9)

#***************************************************************************************************************
#*
#*4) Thermal Endurance. To understand the relationship between temperature and lifetime of polyester 
#enameled wire, thermal endurance tests were performed ("Thermal Endurance of Polyester Enameled 
#Wires Using Twisted Wire Specimens," IEEE Trans. Insulation, 1965: 38-44).
#Data (ex13.19).
#What type of probabilistic relationship between lifetime and temperature does the scatter plot of the 
#data suggest? 
#Fit a ln(Lifetime) = f(Temp) model and summarize the p-values of the coefficients.
#Predict the Lifetime for a Temp = 230

ex13.19
plot(Lifetime~Temp,data=ex13.19)
plot(log(Lifetime)~Temp,data=ex13.19)
fit <- lm(log(Lifetime)~Temp,data=ex13.19)
summary(fit)

#**********************************************************************************
