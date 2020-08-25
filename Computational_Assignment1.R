#Computational Assignment 1

data = read.csv('/Users/poojadeshpande91/Desktop/NU/MSDS 410/Assignments/Computational Assignment 1/USStates.csv')

#General EDA
head(data)
str(data)
summary(data)
dim(data)

#Householdincome can be Y, HighSchool, College, NonWhite, State, Insured can be X
#Obese can be Y, PhysicalActivity, HeavyDrinkers,Smokers can be X
#Insured can be Y, HouseholdIncome, HighSchool, College can be X
#May be PhysicalActivity can be Y and Obese can be X

#Question 2: 
library(moments)
basic_statistics_discrete <- function(x){
  cat(summary(x))
}
basic_statistics_continuous <- function(x){
  cat("\n")
  cat(summary(x))
  cat("\n")
  cat("Standard Deviation : ")
  cat(sd(as.numeric(unlist(x))))
  cat("\n")
  cat("Skewness : ")
  cat(skewness(x))
  cat("\n")
  cat("Kurtosis : ")
  cat(kurtosis(x))
}
cols <- colnames(data)
for (col in cols){
  if (is.factor(data[,col])){
    cat("\n\n")
    cat(col)
    cat("\n")
    basic_statistics_discrete(data[,col])
  }
  else{
    cat("\n\n")
    cat(col)
    basic_statistics_continuous(data[col])
  }
}

#HouseholdIncome plots
library(car)
library(ggplot2)
library(gridExtra)
with(data,{hist(HouseholdIncome,breaks = 10,freq = FALSE,col = "LightBlue")
           lines(density(HouseholdIncome),lwd = 3,lty = 2)
           lines(adaptiveKernel(HouseholdIncome),lwd = 2)
           rug(HouseholdIncome)
           legend("topright",c("Fixed Bandwidth","Adaptive Bandwidth"),lty = 2:1)
           box()})

p2 <- ggplot(data,aes(sample = HouseholdIncome))+
  stat_qq(color = "seagreen")+
  geom_qq_line(color = "seagreen")
p3 <- ggplot(data,aes(x = 1,y = HouseholdIncome))+
    geom_boxplot(fill = "hotpink4")
grid.arrange(grobs = list(p2,p3),ncol = 2,top = "HouseholdIncome")

#Plots with the response variable
#Population
ggplot(data,aes(x = Population,y = HouseholdIncome))+
  geom_point(color = "indianred4")+
  geom_smooth(method = 'lm',color = "indianred3")+
  ggtitle("HouseholdIncome vs Population")
#Region
ggplot(data,aes(x = Region,y = HouseholdIncome))+
  geom_boxplot(fill = "hotpink2")+
  ggtitle("HouseholdIncome vs Region")
#State
ggplot(data,aes( State,HouseholdIncome))+
  geom_bar(stat = "identity",fill = "tomato3")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("HouseholdIncome vs State")
#Non-demographic variable plots with HouseholdIncome
p1 <- ggplot(data,aes(HighSchool,HouseholdIncome))+
  geom_point(color="orangered1")+
  geom_smooth(method = "lm",color = "orangered2")+geom_rug()
p2 <- ggplot(data,aes(College,HouseholdIncome))+
  geom_point(color="darkseagreen4")+
  geom_smooth(method = "lm",color = "darkseagreen4")+
  geom_rug()
p3 <- ggplot(data,aes(Smokers,HouseholdIncome))+
  geom_point(color="orangered1")+
  geom_smooth(method = "lm",color = "orangered2")+
  geom_rug()
p4 <- ggplot(data,aes(PhysicalActivity,HouseholdIncome))+
  geom_point(color="darkseagreen4")+
  geom_smooth(method = "lm",color = "darkseagreen4")+
  geom_rug()
p5 <- ggplot(data,aes(Obese,HouseholdIncome))+
  geom_point(color="orangered1")+
  geom_smooth(method = "lm",color = "orangered2")+
  geom_rug()
p6 <- ggplot(data,aes(NonWhite,HouseholdIncome))+
  geom_point(color="darkseagreen4")+
  geom_smooth(method = "lm",color = "darkseagreen4")+
  geom_rug()
p7 <- ggplot(data,aes(HeavyDrinkers,HouseholdIncome))+
  geom_point(color="orangered1")+
  geom_smooth(method = "lm",color = "orangered2")+
  geom_rug()
p8 <- ggplot(data,aes(TwoParents,HouseholdIncome))+
  geom_point(color="darkseagreen4")+
  geom_smooth(method = "lm",color = "darkseagreen4")+
  geom_rug()
p9 <- ggplot(data,aes(Insured,HouseholdIncome))+
  geom_point(color="orangered1")+
  geom_smooth(method = "lm",color = "orangered2")+
  geom_rug()
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3,ncol=3,top = "Scatterplots for non demographic variables")


#Question 3: Correlation coefficients and Heatmap
library(reshape2)
subdat <- data[,4:13]
mcor <- cor(subdat)
mcor
melted <- melt(mcor)
melted
#Heatmap
ggplot(melted,aes(x = Var1,y = Var2,fill = value))+
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_gradient2(low = "steelblue", high = "tomato1", mid = "white",limit = c(-1,1))+
  ggtitle("Heatmap")
#Write the correlation matrix to an Excel file
library(xlsx)
write.xlsx(mcor, "./correlation.xlsx")

#Question 4: Model with only COLLEGE variable
model1 <- lm(HouseholdIncome~College,data = data)
summary(model1)  
coef(model1)
confint(model1)
anova(model1)
#residual plots
plot(model1)

#Regression plot
ggplot(data, aes(College, HouseholdIncome))+
  geom_point(color = 'seagreen',size = 2) +
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_line(color = 'seagreen4',data = fortify(model1), aes(x = College, y = .fitted),lwd = 1.5)+
  ggtitle("Model 1 Regression plot")

#Verification of coefficient and intercept by hand
y <- data$HouseholdIncome
y_bar <- mean(y)
x <- data$College
x_bar <- mean(x)
numerator <- sum((y-y_bar)*(x-x_bar))
denominator <- sum((x-x_bar)^2)
b1 <- numerator/denominator
b1

b0 <- y_bar - (b1*x_bar)
b0

#Question 5 - Comparing manual calculations with ANOVA table
y <- data$HouseholdIncome
y_fitted <- 23.0664 + 0.9801*data$College
residuals <- y - y_fitted
squared_residuals <- residuals^2
sum_squared_residuals <- sum(squared_residuals)
y_bar <-mean(y)
mean_deviations <- y - y_bar
sum_squares_total <- sum(mean_deviations^2)
fitted_deviations <- y_fitted - y_bar
sum_squares_regression <- sum(fitted_deviations^2)
r_squared <- sum_squares_regression/sum_squares_total
cat("r_squared : ",r_squared)
cat("sum_squares_regression : ",sum_squares_regression)
cat("sum_squared_residuals : ",sum_squared_residuals)

#Question 6  - Model2
model2 <- lm(HouseholdIncome~College+Insured,data = data)
summary(model2)
coef(model2)
confint(model2)
anova(model2)
#plot the residuals
plot(model2)

#Question 7 
model3 <- lm(HouseholdIncome~College+Insured+Smokers,data = data)
summary(model3)

model4 <- lm(HouseholdIncome ~ College+Insured+Smokers+Obese,data = data)
summary(model4)

model5 <- lm(HouseholdIncome~College+Insured+Smokers+Obese+TwoParents,data = data)
summary(model5)

model6 <- lm(HouseholdIncome~College+Insured+Smokers+Obese+TwoParents+PhysicalActivity,data = data)
summary(model6)

model7 <- lm(HouseholdIncome~College+Insured+Smokers+Obese+TwoParents+PhysicalActivity+HighSchool,data = data)
summary(model7)

model8 <- lm(HouseholdIncome~College+Insured+Smokers+Obese+TwoParents+PhysicalActivity+HighSchool+HeavyDrinkers,data = data)
summary(model8)

model9 <- lm(HouseholdIncome~College+Insured+Smokers+Obese+TwoParents+PhysicalActivity+HighSchool+HeavyDrinkers+NonWhite,data = data)
summary(model9)

#Question 8
model <- lm(HouseholdIncome~College+Smokers+HighSchool+NonWhite,data=data)
summary(model)

#Question 9 
#Backward selection
mod1 <- lm(HouseholdIncome~College+Insured+Smokers+Obese+TwoParents+PhysicalActivity+HighSchool+HeavyDrinkers+NonWhite,data = data)
summary(mod1)

#Build a new model without Insured as it has the highest p-value
mod2 <- lm(HouseholdIncome~College+Smokers+Obese+TwoParents+PhysicalActivity+HighSchool+HeavyDrinkers+NonWhite,data = data)
summary(mod2)

#Build a new model without PhysicalActivity as it has the highest p-value
mod3 <- lm(HouseholdIncome~College+Smokers+Obese+TwoParents+HighSchool+HeavyDrinkers+NonWhite,data = data)
summary(mod3)

#Build a new model without Obese as it has the highest p-value
mod4 <- lm(HouseholdIncome~College+Smokers+TwoParents+HighSchool+HeavyDrinkers+NonWhite,data = data)
summary(mod4)

#Build a new model without HighSchool
mod5 <- lm(HouseholdIncome~College+Smokers+TwoParents+HeavyDrinkers+NonWhite,data = data)
summary(mod5)

#Build a new model without Smokers
mod6 <- lm(HouseholdIncome~College+TwoParents+HeavyDrinkers+NonWhite,data = data)
summary(mod6)


