data <- read.csv("/Users/poojadeshpande91/Desktop/NU/MSDS 410/Assignments/Assignment_1/ames_housing_data.csv")
options(scipen=999)

head(data)
str(data)
summary(data)
data$SubClass <- as.factor(data$SubClass)

#Typical home : 
#Sale condition : Normal
#Zoning - RL,RH, RM, FV
#Bldg Type - 1Fam, Twnhs, TwnhsE
#GrLivArea > 4000
col_to_choose <- c("Normal")
initial_length <- dim(data)[1]
after_sale_condition <- data[data[,"SaleCondition"] %in% col_to_choose,]
after_sale_condition_length <- dim(after_sale_condition)[1]

col_to_choose <- c("RL","RM","RH","FV")
after_zoning <- after_sale_condition[after_sale_condition[,"Zoning"] %in% col_to_choose, ]
after_zoning_length <- dim(after_zoning)[1]

col_to_choose <- c("1Fam","Twnhs","TwnhsE")
after_bldgtype <- after_zoning[after_zoning[,"BldgType"] %in% col_to_choose,]
after_bldgtype_length <- dim(after_bldgtype)[1]

after_grdlivarea <- after_bldgtype[after_bldgtype["GrLivArea"] <= 4000,]
after_grdlivarea_length <- dim(after_grdlivarea)[1]

col_to_choose <- c("1Story","2Story","1.5Fin","SFoyer","SLvl")
after_housestyle <- after_grdlivarea[after_grdlivarea[,"HouseStyle"] %in% col_to_choose,]
after_housestyle_length <- dim(after_housestyle)[1]

summary(after_housestyle$SubClass)
col_to_choose <- c(20,30,50,60,70,80,85,90,120,160,190)
after_subclass <- after_housestyle[after_housestyle[,"SubClass"] %in% col_to_choose,]
after_subclass_length <- dim(after_subclass)[1]



#Waterfall chart
waterfall <- data.frame(columns = c("InitialDataframe","SaleCondition","Zoning",
                                    "BldgType","GrLivArea","HouseStyle",'SubClass'),
                        end = c(initial_length,after_sale_condition_length,after_zoning_length,
                                after_bldgtype_length,after_grdlivarea_length,after_housestyle_length,
                                after_subclass_length))
waterfall$columns <- factor(waterfall$columns,levels=c("InitialDataframe","SaleCondition",
                                                       "Zoning","BldgType","GrLivArea","HouseStyle",
                                                       "SubClass"))
waterfall$start <- c(0,head(waterfall$end,-1))
waterfall$amount <- waterfall$end - waterfall$start
waterfall$type <- ifelse(waterfall$amount > 0,"add","drop")
waterfall$id <- seq(dim(waterfall)[1])

#Plot the Waterfall chart
library(ggplot2)
library(gridExtra)
ggplot(waterfall,aes(x=columns,fill = type)) + 
  geom_rect(aes(xmin = id-0.45,xmax = id+0.45,ymin = end, ymax = start))+ 
  scale_x_discrete("", breaks = levels(waterfall$columns)) + 
  geom_text(subset(waterfall,type=="add"),mapping = aes(columns,end,label = amount),vjust = 1)+
  geom_text(subset(waterfall,type=="drop"),mapping = aes(columns,end,label = amount),vjust = -0.5)+
  ggtitle("Waterfall chart for the dropped values")+ylab("No. of Observations")+xlab("Column Names")

#Final dataframe
final_df <- after_subclass
dim(final_df)[1]


#Before drop conditions
p1 <- ggplot(data,aes(SalePrice))+geom_histogram(color = "Black",fill  = "Blue",bins = 20)
p2 <- ggplot(data,aes(sample=SalePrice))+stat_qq(color = "seagreen1")+geom_qq_line(color = "seagreen4")
p3 <- ggplot(data,aes(x=1,y=SalePrice))+geom_boxplot(fill = "indianred2")
grid.arrange(p1,p2,p3,top = "Sale Price",nrow = 2, ncol = 2)

#Seeing outliers in SalePrice

p1 <- ggplot(final_df,aes(SalePrice))+geom_histogram(color = "Black",fill  = "Blue",bins = 20)
p2 <- ggplot(final_df,aes(sample=SalePrice))+stat_qq(color = "seagreen1")+geom_qq_line(color = "seagreen4")
p3 <- ggplot(final_df,aes(x=1,y=SalePrice))+geom_boxplot(fill = "indianred2")
grid.arrange(p1,p2,p3,top = "Sale Price",nrow = 2,ncol = 2)

summary(final_df$SalePrice)
q <- quantile(final_df$SalePrice,probs = c(0.25,0.75),names = FALSE)
iqr <- IQR(final_df$SalePrice)
extreme_outlier_bound <- q[2]+3.0*iqr
extreme_outlier_bound
num_extreme_outliers <- dim(final_df[final_df[,"SalePrice"]>extreme_outlier_bound,])[1]
num_extreme_outliers

#Deleting outliers
final_df <- final_df[final_df[,"SalePrice"]<extreme_outlier_bound,]
dim(final_df)[1]

#Data Quality
library(moments)
colnames(final_df)
#SubClass, Zoning, LotArea, Utilities, Neighborhood, Condition1, BldgType,
#HouseStyle, OverallQual, OverallCond, YearBuilt, ExterQual, ExterCond,
#TotalBsmtSF, HeatingQC, CentralAir, FirstFlrSF, SecondFlrSF, BedroomAbvGr, KitchenQual
quality_discrete <- function(x){
  cat(summary(x))
}
quality_continuous <- function(x){
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
#Twenty columns for Data quality check
cols <- c("SubClass","Zoning","LotArea","Utilities","Neighborhood",
          "Condition1","BldgType","HouseStyle","OverallQual",
          "OverallCond","YearBuilt","ExterQual","ExterCond",
          "TotalBsmtSF","HeatingQC","CentralAir","FirstFlrSF",
          "SecondFlrSF","TotRmsAbvGrd","KitchenQual")

#Data Quality check for 20 variables
for (col in cols){
  if (is.factor(final_df[,col])){
    cat("\n\n")
    cat(col)
    cat("\n")
    quality_discrete(final_df[col])
  }
  else{
    cat("\n\n")
    cat(col)
    quality_continuous(final_df[col])
  }
}

#Exploratory Data Analysis for ten variables
#FirstFlrSF, SecondFlrSF, LotArea,, TotalBsmtSF,
#Neighborhood, OverallCond, YearBuilt, SubClass, TotRmsAbvGrd, OverallQual

#Continuous
p1 <- ggplot(final_df,aes(FirstFlrSF))+geom_histogram(bins = 25,color = "Black",fill = "Blue")+
  ggtitle("First Floor SqFt")
p2 <- ggplot(final_df,aes(SecondFlrSF))+geom_histogram(bins = 25,color = "Black",fill="Red")+
  ggtitle("Second Floor Sqft")
p3 <- ggplot(final_df,aes(LotArea))+geom_histogram(bins = 25,color = "Black",fill="Green")+
  ggtitle("Lot Area")
p4 <- ggplot(final_df,aes(TotalBsmtSF))+geom_histogram(bins = 25, color = "Black",fill = "Brown")+
  ggtitle("Basement Sqft")

#Histograms
library(gridExtra)
grid.arrange(p1,p2,p3,p4)


#QQPLot
p1 <- ggplot(final_df,aes(sample = FirstFlrSF))+stat_qq(color = "steelblue4")+geom_qq_line()+
  ggtitle("First Flr SQft")
p2 <- ggplot(final_df,aes(sample = SecondFlrSF))+stat_qq(color = "palevioletred")+geom_qq_line()+
  ggtitle("Second Flr SQft")
p3 <- ggplot(final_df,aes(sample = LotArea))+stat_qq(color = "hotpink4")+geom_qq_line()+
  ggtitle("Lot Area")
p4 <- ggplot(final_df,aes(sample = TotalBsmtSF))+stat_qq(color = "magenta4")+geom_qq_line()+
  ggtitle("Basement Sqft")
grid.arrange(p1,p2,p3,p4)

#Boxplot
p1 <- ggplot(final_df,aes(x=1,y= FirstFlrSF))+geom_boxplot(fill = "paleturquoise")+
  ggtitle("First Flr SQft")
p2 <- ggplot(final_df,aes(x=1,y=SecondFlrSF))+geom_boxplot(fill="palevioletred")+
  ggtitle("Second Flr SQft")
p3 <- ggplot(final_df,aes(x=1,y=LotArea))+geom_boxplot(fill = "hotpink4")+
  ggtitle("Lot Area")
p4 <- ggplot(final_df,aes(x=1,y=TotalBsmtSF))+geom_boxplot(fill = "blue")+
  ggtitle("Basement Sqft")
grid.arrange(p1,p2,p3,p4)


#Scatterplot
ggplot(final_df,aes(x = FirstFlrSF,y = SecondFlrSF))+geom_point(color = "Brown")+
  ggtitle("FirstFlrSF vs SecondFlrSF")

#Scatterplot
ggplot(final_df,aes(x = LotArea,y = FirstFlrSF))+geom_point(color = "Blue")


#Discrete
p1 <- ggplot(final_df,aes(OverallQual))+geom_bar(fill = "indianred4")+scale_x_discrete(limits = seq(1:10))+
  ggtitle("Overall Quality")
p2 <-ggplot(final_df,aes(OverallCond))+geom_bar(fill = "indianred3")+scale_x_discrete(limits = seq(1:10))+
  ggtitle("Overall Condition")
p3 <- ggplot(final_df,aes(YearBuilt))+geom_bar(fill = "indianred2")+
  ggtitle("Year Built")
p4 <- ggplot(final_df,aes(TotRmsAbvGrd))+geom_bar(fill = "indianred")+scale_x_discrete(limits = seq(1:13))+
  ggtitle("Total Rooms")
grid.arrange(p1,p2,p3,p4)

#Categorical variables
#SubClass, Zoning, Street, Alley, LotShape, LandContour, Utilities
#LotConfig, LandSlope, Neighborhood, Condition1, Condition2, BldgType
#HouseStyle, OverallQual, OverallCond, RoofStyle, RoofMatl, Exterior1, Exterior2
#MsVnrType, ExterQual, ExterCond, Foundation, BsmtQual, BsmtCond,
#BsmtExposure, BsmtFinType1, BsmtFinType2, Heating, CentralAir, 
#Electrical, KitchenQual, Functional, FireplaceQu, GarageType, GarageFinish,
#GarageQual, GarageCond, PavedDrive, PoolQC, Fence, MiscFeature, SaleType,
#SaleCond

#To consider
#Zoning, Street, Alley, Utilities, LotConfig, Neighborhood, BldgType, HouseStyle,
#BsmtQual, BsmtCond, CentralAir, KitchenQual, GarageFinish, PoolQC, MiscFeature,OverallQUal, OverallCond

cat_var <- c("Zoning", "Street", "Alley", "Utilities", "LotConfig", "Neighborhood", "BldgType", 
             "HouseStyle","BsmtQual", "BsmtCond", "CentralAir", "KitchenQual", "GarageFinish", 
             "PoolQC", "MiscFeature","OverallQual","OverallCond")
r_squared <- c()
for (var in cat_var){
  model <- lm(final_df$SalePrice ~ final_df[,var])
  sum <-summary(model)
  r_squared <- append(r_squared,round(sum$r.squared,3))
}
categorical <- data.frame(cat_var,r_squared)
categorical <-categorical[order(-r_squared),]
categorical

chosen_cat <- c("OverallQual","PoolQC","Neighborhood","BsmtQual","KitchenQual")
for (var in chosen_cat){
  cat(var)
  cat("\n")
  print(aggregate(final_df$SalePrice, by=list(Category=final_df[,var]), FUN=mean))
  cat("\n")
  
}

#Pool Yes, Pool No - dummy variable

final_df$Pool <- ifelse(is.na(final_df$PoolQC),0,1)
aggregate(SalePrice~Pool,final_df,mean)

#Changed all NA values to a category - 'No Basement'
final_df$BsmtQual <-addNA(final_df$BsmtQual)
levels(final_df$BsmtQual) <- c(levels(final_df$BsmtQual),"No")
final_df$BsmtQual[is.na(final_df$BsmtQual)] <- "No"

#BsmtQual - 6 categories including 'No Basement'. We need 5 dummy coded variables.
#Default is the one with excellent rating 'No'
final_df$BsmtQual_Ex <- ifelse((final_df$BsmtQual=='Ex'),1,0)
final_df$BsmtQual_Ta <- ifelse((final_df$BsmtQual=='TA'),1,0)
final_df$BsmtQual_Po <- ifelse((final_df$BsmtQual=='Po'),1,0)
final_df$BsmtQual_Gd <- ifelse((final_df$BsmtQual=='Gd'),1,0)
final_df$BsmtQual_Fa <- ifelse((final_df$BsmtQual=='Fa'),1,0)

final_df[,c("BsmtQual","BsmtQual_Ex","BsmtQual_Ta","BsmtQual_Po","BsmtQual_Gd","BsmtQual_Fa")]

#KitchenQual has 5 categories. So we need 4 dummy variables.
#Default is the one with poor rating - 'Po'
final_df$KitchenQual_Ex <- ifelse(final_df$KitchenQual=='Ex',1,0)
final_df$KitchenQual_Ta <- ifelse(final_df$KitchenQual=='TA',1,0)
final_df$KitchenQual_Gd <- ifelse(final_df$KitchenQual=='Gd',1,0)
final_df$KitchenQual_Fa <- ifelse(final_df$KitchenQual=='Fa',1,0)
final_df[,c("KitchenQual","KitchenQual_Ex","KitchenQual_Ta","KitchenQual_Gd","KitchenQual_Fa")]
summary(final_df$PoolQC)


#Q.2
set.seed(123)
final_df$u <- runif(n=dim(final_df)[1],min = 0 , max = 1)

# Define these  variables for later use;
final_df$QualityIndex <- final_df$OverallQual*final_df$OverallCond;
final_df$TotalSqftCalc <- final_df$BsmtFinSF1+final_df$BsmtFinSF2+final_df$GrLivArea
final_df$Age <- final_df$YrSold - final_df$YearBuilt
final_df$Remodel <- ifelse(final_df$YearRemodel==final_df$YearBuilt,0,1)
  
train_df <- subset(final_df,u<0.7)
test_df <- subset(final_df,u>=0.7)

dim(final_df)[1]
dim(train_df)[1]
dim(test_df)[1]
dim(train_df)[1]+dim(test_df)[1]

#Q.3
#Predictor variables:
#QualityIndex, TotalSqftCalc, Age, Pool, BsmtQual(dummy), KitchenQual(dummy),
#Remodel,TotRmsAbvGrd, Fireplaces

#Added two more variables - Age and dummy variable Remodel.
cols_to_keep <- c("SalePrice","QualityIndex", "TotalSqftCalc", "Age", "Pool","BsmtQual_Ex","BsmtQual_Ta","BsmtQual_Gd","BsmtQual_Fa",
                  "BsmtQual_Po","KitchenQual_Ex","KitchenQual_Gd","KitchenQual_Fa","KitchenQual_Ta","Remodel","TotRmsAbvGrd",
                  "Fireplaces")
train.clean <-train_df[,(names(final_df) %in% cols_to_keep)]
dim(train.clean)

#Use stepAIC() for automated variable selection. 
#You need to define the upper and lower models here which are basically the full model and a model with no predictors
upper.lm <- lm(SalePrice~.,train.clean)
lower.lm <- lm(SalePrice~1,train.clean)

#You also need a simple linear regression model to initialize the variable selection.
sqft.lm <- lm(SalePrice~TotalSqftCalc,train.clean)

library(MASS)
help("stepAIC")

#: use the R function formula() to pass your shortcut definition of the 
#Full Model to the scope argument in stepAIC().  
forward.lm <-stepAIC(object = lower.lm, scope = list(upper = formula(upper.lm),lower = ~1),
                     direction = c("forward"))
summary(forward.lm)

backward.lm <- stepAIC(object = upper.lm,direction = c("backward"))
summary(backward.lm)

stepwise.lm <- stepAIC(object = sqft.lm,scope = list(upper = formula(upper.lm),lower = ~1),
                       direction = c('both'))
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train_df)
summary(junk.lm)
#One issue with using variable selection on a pool that contains highly correlated predictor 
#variables is that the variable selection algorithm will select the highly correlated pairs.

# Compute the VIF values
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)

#Metrics for Training data
#Retrieve Adjusted R-square from summary()

names(summary(stepwise.lm))
r_square <- c(summary(forward.lm)$adj.r.squared, summary(backward.lm)$adj.r.squared,
              summary(stepwise.lm)$adj.r.squared, summary(junk.lm)$adj.r.squared)
aic <- c(AIC(forward.lm),AIC(backward.lm),AIC(stepwise.lm),AIC(junk.lm))
bic <- c(BIC(forward.lm),BIC(backward.lm),BIC(stepwise.lm),BIC(junk.lm))

mse_forward <- mean((summary(forward.lm)$residuals)^2)
mse_backward <- mean((summary(backward.lm)$residuals)^2)
mse_stepwise <- mean((summary(stepwise.lm)$residuals)^2)
mse_junk <- mean((summary(junk.lm)$residuals)^2)

mse_values <- c(mse_forward,mse_backward,mse_stepwise,mse_junk)
mse_values
mae_forward <- mean(abs(summary(forward.lm)$residuals))
mae_backward <- mean(abs(summary(backward.lm)$residuals))
mae_stepwise<- mean(abs(summary(stepwise.lm)$residuals))
mae_junk <- mean(abs(summary(junk.lm)$residuals))

mae_values <- c(mae_forward,mae_backward,mae_stepwise,mae_junk)
model_names <-  c("Forward selection","Backward selection","Stepwise Selection","Junk")
training_metrics <- data.frame(model_names,r_square,aic,bic,mse_values,mae_values)
colnames(training_metrics) <- c("Model Name","Adjusted R-squared","AIC","BIC","MSE","MAE")
training_metrics
summary(forward.lm)$residuals
#Q.4

#Find test case predictions
forward.test <- predict(forward.lm,newdata=test_df)
backward.test <- predict(backward.lm,newdata=test_df)
stepwise.test <- predict(stepwise.lm,newdata=test_df)
junk.test <- predict(junk.lm,newdata=test_df)

#Calculate MSE for test dataset
mse_test_forward <- mean((forward.test-test_df$SalePrice)^2)
mse_test_backward <- mean((backward.test-test_df$SalePrice)^2)
mse_test_stepwise <- mean((stepwise.test-test_df$SalePrice)^2)
mse_test_junk <- mean((junk.test-test_df$SalePrice)^2)

mse_test <- c(mse_test_forward,mse_test_backward,mse_test_stepwise,mse_test_junk)

#Calculate mae for test dataset
mae_test_forward <- mean(abs(forward.test-test_df$SalePrice))
mae_test_backward <- mean(abs(backward.test-test_df$SalePrice))
mae_test_stepwise <- mean(abs(stepwise.test-test_df$SalePrice))
mae_test_junk <- mean(abs(junk.test-test_df$SalePrice))

mae_test <- c(mae_test_forward,mae_test_backward,mae_test_stepwise,mae_test_junk)

data.frame(model_names,mse_test,mae_test)

#Q.5
# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;

# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable
forward.trainTable/sum(forward.trainTable)

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test_df$SalePrice-forward.test)/test_df$SalePrice
backward.testPCT <- abs(test_df$SalePrice-backward.test)/test_df$SalePrice
stepwise.testPCT <- abs(test_df$SalePrice-stepwise.test)/test_df$SalePrice
junk.testPCT <- abs(test_df$SalePrice-junk.test)/test_df$SalePrice

# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)

stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)

junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)

#Q.6
summary(backward.lm)

#None of the coefficients have weird signs. No real evidence of multicollinearity

#Current R-square = 0.8611, adjusted = 0.8602
#Remove TotRmsAbvGrd

m1 <- lm(formula = SalePrice ~   Fireplaces + BsmtQual_Ex + 
     BsmtQual_Ta + BsmtQual_Gd + KitchenQual_Ex + KitchenQual_Gd + 
     QualityIndex + TotalSqftCalc + Age, data = train.clean)
summary(m1)

#R-square = 0.8497, adjusted R-square = 0.8488. We want to keep TotRmsAbvGrd. Try removing Fireplaces

m2 <- lm(formula = SalePrice ~  TotRmsAbvGrd  + BsmtQual_Ex + 
           BsmtQual_Ta + BsmtQual_Gd + KitchenQual_Ex + KitchenQual_Gd + 
           QualityIndex + TotalSqftCalc + Age, data = train.clean)
summary(m2)

#R-square = 0.8551, adjusted = 0.8543. So I will remove Fireplaces.  Try removing age
m3 <- lm(formula = SalePrice ~  TotRmsAbvGrd   + BsmtQual_Ex + 
           BsmtQual_Ta + BsmtQual_Gd + KitchenQual_Ex + KitchenQual_Gd + 
           QualityIndex + TotalSqftCalc , data = train.clean)
summary(m3)
#R-square = 0.8408, adjusted = 0.84. Causes a change. So remove

#With all the dummy variables for the categorical variables.
m4 <- lm(formula = SalePrice ~  TotRmsAbvGrd  + BsmtQual_Ex + 
           BsmtQual_Ta + BsmtQual_Gd + BsmtQual_Fa + BsmtQual_Po + KitchenQual_Ex + KitchenQual_Gd + KitchenQual_Fa +KitchenQual_Ta+
           QualityIndex + TotalSqftCalc , data = train.clean)
summary(m4)

#Include the categorical variables directly
train.clean$BsmtQual <- train_df$BsmtQual
train.clean$KitchenQual <- train_df$KitchenQual

#Try interaction terms
m5 <- lm(formula = SalePrice ~  TotRmsAbvGrd  + BsmtQual + KitchenQual +
           QualityIndex + TotalSqftCalc + BsmtQual*QualityIndex + KitchenQual*QualityIndex, data = train.clean)
summary(m5)

#KitchenQual*QualityIndex is not significant. So only include BsmtQual*QualityIndex.
m6 <- lm(formula = SalePrice ~  TotRmsAbvGrd  + BsmtQual + KitchenQual +
           QualityIndex + TotalSqftCalc + BsmtQual*QualityIndex , data = train.clean)
summary(m6)
anova(m6)
interaction.plot(train.clean$QualityIndex,train.clean$BsmtQual,train.clean$SalePrice)
#Plots
standardized_residuals <-rstandard(m6)
hist(standardized_residuals,col = 'blue')
qqnorm(standardized_residuals, pch = 1, frame = FALSE)
qqline(standardized_residuals, col = "steelblue", lwd = 2)
residualPlots(m6)
influenceIndexPlot(m6)

dim(train.clean)
rownames(train.clean)
train.clean[rownames(train.clean)==2893,]
summary(train.clean)
cooks.distance(m6)[names(cooks.distance(m6))=="2893"]


mean(cooks.distance(m6))
rownames.toremove <- c("2893")
train.clean <- train.clean[!(rownames(train.clean) %in% rownames.toremove),]
m6 <- lm(formula = SalePrice ~  TotRmsAbvGrd  + BsmtQual + KitchenQual +
           QualityIndex + TotalSqftCalc + BsmtQual*QualityIndex , data = train.clean)
summary(m6)
influenceIndexPlot(m6)
