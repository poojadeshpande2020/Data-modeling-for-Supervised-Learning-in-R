#Import libraries
library(ggplot2)
library(gridExtra)
library(car)

#Reading the data
data = read.csv('/Users/poojadeshpande91/Desktop/NU/MSDS 410/Assignments/Computational Assignment 3/NutritionStudy.csv')

summary(data)
str(data)
dim(data)

#Q.1
cor.test(data$Cholesterol,data$Fiber,method='pearson')
ggplot(data,aes(x=Fiber,y=Cholesterol))+geom_point(color = 'tomato')+
  ggtitle("Fiber vs Cholesterol")+geom_smooth(color = "tomato4",method = 'lm')

#Q.2
model1 <- lm(Cholesterol~Fiber,data = data)

summary(model1)
anova(model1)
confint(model1)

standardized_res <- rstandard(model1)
par(mfrow = c(1,2))
hist(standardized_res,col = 'blue')
qqnorm(standardized_res,col = 'steelblue4')
qqline(standardized_res,col='steelblue')
residualPlots(model1)

#Q.3
#Dummy coded variables for Alcohol
data$AlcoholCatg <- ifelse(data$Alcohol==0,0,ifelse((data$Alcohol>0) & (data$Alcohol<10),1,2))


#Indicator dummy coded variables
data$Alcohol1 <- ifelse(data$AlcoholCatg==1,1,0)
data$Alcohol2 <- ifelse(data$AlcoholCatg==2,1,0)

data[1:5,c("Alcohol","AlcoholCatg","Alcohol1","Alcohol2")]

#Model with Fiber and Alcohol Dummy variables
model2 <- lm(Cholesterol~Fiber + Alcohol1 + Alcohol2,data = data)

summary(model2)
anova(model2)

standardized_res <- rstandard(model2)
par(mfrow = c(1,2))
hist(standardized_res,col = 'blue')
qqnorm(standardized_res,col = 'steelblue4')
qqline(standardized_res,col='steelblue')
residualPlots(model2)
influenceIndexPlot(model2)

#Q.4
predicted <- predict(model2,data = data[,c("Fiber","Alcohol1","Alcohol2")])
data$predicted_model2 <- predicted
data$AlcoholCatg <-as.factor(data$AlcoholCatg)
ggplot(data = data, aes(x = Fiber,y = predicted_model2,color = AlcoholCatg))+
  geom_point()+
  ggtitle("Predicted Cholesterol values")+
  ylab("Predicted Cholesterol")

ggplot(data = data, aes(x = Fiber,y = Cholesterol,color = AlcoholCatg))+
  geom_point()+
  ggtitle("Actual Cholesterol values")+
  ylab("Actual Cholesterol")

#Q.5
#Create interaction terms
data$Alcohol1_Fiber <- data$Alcohol1 * data$Fiber
data$Alcohol2_Fiber <- data$Alcohol2 * data$Fiber

data[1:5,c("AlcoholCatg","Alcohol1","Alcohol2","Alcohol1_Fiber","Alcohol2_Fiber","Fiber")]

model3 <- lm(Cholesterol~Fiber+Alcohol1+Alcohol2+Alcohol1_Fiber+Alcohol2_Fiber,data = data)

summary(model3)
anova(model3)
confint(model3)

data$predicted_model3 <- predict(model3,newdata = data[,c("Fiber","Alcohol1","Alcohol2","Alcohol1_Fiber","Alcohol2_Fiber")])

ggplot(data ,aes(x = Fiber,y = predicted_model3,color=AlcoholCatg))+geom_point()+
  ggtitle("Predicted values for Cholesterol")

standardized_res <- rstandard(model3)
par(mfrow = c(1,2))
hist(standardized_res,col = 'blue')
qqnorm(standardized_res,col = 'steelblue4')
qqline(standardized_res,col='steelblue')
residualPlots(model3)
influenceIndexPlot(model3)


#Q.6
anova(model2,model3)


#Q.7
str(data)
model4 <- lm(Cholesterol~Fiber + Smoke + Smoke*Fiber,data = data)
summary(model4)
m4 <- lm(Cholesterol~Fiber  + Smoke,data = data)
anova(m4,model4)

model5 <- lm(Cholesterol~Fiber + VitaminUse + VitaminUse*Fiber,data = data)
summary(model5)
m5 <- lm(Cholesterol~Fiber+VitaminUse,data = data)
anova(m5,model5)

model6 <- lm(Cholesterol~Fiber + Gender + Gender*Fiber,data = data)
summary(model6)
m6 <- lm(Cholesterol~Fiber+Gender,data = data)
anova(m6,model6)




