
library(car)
library(ggplot2)
library(gridExtra)
library(plyr)
data = read.csv('/Users/poojadeshpande91/Desktop/NU/MSDS 410/Assignments/Computational Assignment 3/NutritionStudy.csv')
options(scipen=999)

head(data)
summary(data)
str(data)


#Q.1
#Regular = 1, Occasional = 2, No = 3
data$VitaminRecoded <- revalue(data$VitaminUse,c("Regular" = 1,"Occasional" = 2, "No" = 3))
#Female = 1, Male = 2
data$GenderRecoded <- revalue(data$Gender,c("Female" = 1,"Male" = 2))
#No = 1, Yes = 2
data$SmokeRecoded <- revalue(data$Smoke,c("No" =1, "Yes" = 2))
str(data)
head(data)

#Q.2
#Cholesterol plots
p1 <- ggplot(data = data,aes(x = Cholesterol))+
geom_histogram(color = 'black',fill = 'blue',bins = 20)+
ggtitle("Histogram of Cholesterol")

p2 <- ggplot(data = data,aes(sample= Cholesterol))+
stat_qq(color = 'tomato1')+
geom_qq_line(color = 'tomato4')+
ggtitle("QQ plots of Histogram")

grid.arrange(p1,p2,ncol = 2)

#Cholesterol and Vitamin Use plot
ggplot(data = data,aes(y = Cholesterol,x = VitaminUse))+
geom_boxplot(fill = 'tomato')+
ggtitle("Cholesterol vs VitaminUse")


#Model building
model1 <- lm(Cholesterol~VitaminRecoded,data = data)
summary(model1)
anova(model1)

#After model plots
standardized_residuals <-rstandard(model1)
hist(standardized_residuals,col = 'blue')
residualPlots(model1)
plot(model1)
influenceIndexPlot(model1)

#Recode the categorical variables

data$VitaminRecoded <- revalue(data$VitaminUse,c("Regular" = 3,"Occasional" = 2, "No" = 1))
head(data)
#Rebuild the model
model2 <- lm(Cholesterol~VitaminRecoded,data = data)
summary(model2)
anova(model2)


#Q.3
#Manually create dummy variables with "Regular" as the base class
data$VitaminOccasional <- ifelse(data$VitaminUse=='Occasional',1,0)
data$VitaminNo <- ifelse(data$VitaminUse=='No',1,0)

#Build the model
model3 <- lm(Cholesterol~VitaminOccasional+VitaminNo,data = data)

summary(model3)
anova(model3)

#After model plots
standardized_residuals <-rstandard(model3)
hist(standardized_residuals,col = 'blue')
residualPlots(model3)
plot(model3)
influenceIndexPlot(model3)

#Q.4
#Effect coding with "No" as the comparative group:
#Effect coding uses 1s, 0s and -1s
data$VitaminOcc <- ifelse(data$VitaminUse=='Occasional',1,ifelse(data$VitaminUse=='Regular',0,-1))
data$VitaminReg <- ifelse(data$VitaminUse=='Regular',1,ifelse(data$VitaminUse=='Occasional',0,-1))

#Build the model
model4 <- lm(Cholesterol~VitaminOcc+VitaminReg,data = data)

summary(model4)
anova(model4)

#After model plots
standardized_residuals <-rstandard(model4)
hist(standardized_residuals,col = 'blue')
residualPlots(model4)
plot(model4)
influenceIndexPlot(model4)

#Q.5
#Discretize 'Alcohol' variable 
data$AlcoholCatg <- ifelse(data$Alcohol==0,0,ifelse((data$Alcohol>0) & (data$Alcohol<10),1,2))


#Indicator effect coded variables
data$Alcohol1 <- ifelse(data$AlcoholCatg==1,1,ifelse(data$AlcoholCatg==2,0,-1))
data$Alcohol2 <- ifelse(data$AlcoholCatg==2,1,ifelse(data$AlcoholCatg==1,0,-1))
data[1:5,c("Alcohol","AlcoholCatg","Alcohol1","Alcohol2")]


#Q.6
#Creating interaction variables
data$VitaminOcc_Alcohol1 <- data$VitaminOcc*data$Alcohol1
data$VitaminOcc_Alcohol2 <- data$VitaminOcc*data$Alcohol2
data$VitaminReg_Alcohol1 <- data$VitaminReg*data$Alcohol1
data$VitaminReg_Alcohol2 <- data$VitaminReg*data$Alcohol2

head(data)

#Full model with interaction variables
fullmodel <- lm(Cholesterol~VitaminOcc+VitaminReg+Alcohol1+Alcohol2+VitaminOcc_Alcohol1+VitaminOcc_Alcohol2+
                  VitaminReg_Alcohol1 + VitaminReg_Alcohol2,data =data)
summary(fullmodel)

#Reduced model
reducedmodel <- lm(Cholesterol~VitaminOcc+VitaminReg+Alcohol1+Alcohol2,data = data)
summary(reducedmodel)

#Comparison of reduced and full model
anova(reducedmodel,fullmodel)

#Interaction plot
help("interaction.plot")
interaction.plot(data$AlcoholCatg,data$VitaminUse,data$Cholesterol,
                 fun = mean,col = c("red","green","blue"),
                 xlab ="Alcohol Category",ylab="Mean Cholesterol",
                 pch =c(19,17),type ='b')


#Q.7
#Checking for interaction between Gender and Smoke
interaction.plot(data$Gender,data$Smoke,data$Cholesterol,
                 fun = mean,col = c("red","green","blue"),
                 xlab ="Gender",ylab="Mean Cholesterol",
                 main = "Interaction between Gender and Smoke",
                 pch =c(19,17),type ='b')
fm <- lm(Cholesterol~Gender + Smoke + Gender*Smoke,data = data)
rm <- lm(Cholesterol~Gender+Smoke,data = data)
anova(rm,fm)

#Checking for interaction between Gender and VitaminUse
interaction.plot(data$Gender,data$VitaminUse,data$Cholesterol,
                 fun = mean,col = c("red","green","blue"),
                 xlab ="Gender",ylab="Mean Cholesterol",
                 main = "Interaction between Gender and VItaminUse",
                 pch =c(19,17),type ='b')
fm <- lm(Cholesterol~Gender + VitaminUse + Gender*VitaminUse,data = data)
rm <- lm(Cholesterol~Gender+VitaminUse,data = data)
anova(rm,fm)

#Checking for interaction between Gender and Alcohol
interaction.plot(data$Gender,data$AlcoholCatg,data$Cholesterol,
                 fun = mean,col = c("red","green","blue"),
                 xlab ="Gender",ylab="Mean Cholesterol",
                 main = "Interaction between Gender and Alcohol",
                 pch =c(19,17),type ='b')
fm <- lm(Cholesterol~Gender + AlcoholCatg + Gender*AlcoholCatg,data = data)
rm <- lm(Cholesterol~Gender+AlcoholCatg,data = data)
anova(rm,fm)

#Checking for interaction between Smoke and Alcohol
interaction.plot(data$Smoke,data$AlcoholCatg,data$Cholesterol,
                 fun = mean,col = c("red","green","blue"),
                 xlab ="Smoke",ylab="Mean Cholesterol",
                 main = "Interaction between Smoke and Alcohol",
                 pch =c(19,17),type ='b')
fm <- lm(Cholesterol~Smoke + AlcoholCatg + Smoke*AlcoholCatg,data = data)
rm <- lm(Cholesterol~Smoke+AlcoholCatg,data = data)
anova(rm,fm)

#Checking for interaction between Smoke and VitaminUse
interaction.plot(data$Smoke,data$VitaminUse,data$Cholesterol,
                 fun = mean,col = c("red","green","blue"),
                 xlab ="Smoke",ylab="Mean Cholesterol",
                 main = "Interaction between Smoke and VitaminUse",
                 pch =c(19,17),type ='b')
fm <- lm(Cholesterol~Smoke + VitaminUse + Smoke*VitaminUse,data = data)
rm <- lm(Cholesterol~Smoke+VitaminUse,data = data)
anova(rm,fm)





