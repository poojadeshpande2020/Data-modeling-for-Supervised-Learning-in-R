
data <- read.csv("/Users/poojadeshpande91/Desktop/NU/MSDS 410/Assignments/Assignment_1/ames_housing_data.csv")
options(scipen=999)

head(data)
str(data)
summary(data)


#Typical home : 
#Sale condition : Normal
#Zoning - RL,RH, RM, FV
#HouseStyle : 1Story, 2Story, 1.5Fin, SLvl, SFoyer
#SubClass :Keep with value count greater than 100
col_to_choose <- c("Normal")
initial_length <- dim(data)[1]
after_sale_condition <- data[data[,"SaleCondition"] %in% col_to_choose,]
after_sale_condition_length <- dim(after_sale_condition)[1]

col_to_choose <- c("RL","RM","RH","FV")
after_zoning <- after_sale_condition[after_sale_condition[,"Zoning"] %in% col_to_choose, ]
after_zoning_length <- dim(after_zoning)[1]

col_to_choose <- c("1Story","2Story","1.5Fin","SFoyer","SLvl")
after_housestyle <- after_zoning[after_zoning[,"HouseStyle"] %in% col_to_choose,]
after_housestyle_length <- dim(after_housestyle)[1]

summary(after_housestyle$SubClass)
col_to_choose <- c(20,30,50,60,70,80,85,90,120,160,190)
after_subclass <- after_housestyle[after_housestyle[,"SubClass"] %in% col_to_choose,]
after_subclass_length <- dim(after_subclass)[1]

#Waterfall chart
waterfall <- data.frame(columns = c("InitialDataframe","SaleCondition","Zoning",
                                    "HouseStyle","SubClass"),
                        end = c(initial_length,after_sale_condition_length,after_zoning_length,
                                after_housestyle_length,after_subclass_length))
waterfall$columns <- factor(waterfall$columns,levels=c("InitialDataframe","SaleCondition",
                                                       "Zoning","HouseStyle","SubClass"))
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

final_df[final_df[,"LotArea"]>150000,]
#Scatterplot
ggplot(final_df,aes(x = FirstFlrSF,y = SecondFlrSF))+geom_point(color = "Brown")+
  ggtitle("FirstFlrSF vs SecondFlrSF")

#Scatterplot
ggplot(final_df,aes(x = LotArea,y = FirstFlrSF))+geom_point(color = "Blue")

##New columns
final_df$age <- final_df$YrSold - final_df$YearBuilt
final_df$TotalSqft <- final_df$FirstFlrSF + final_df$SecondFlrSF 
final_df$qual_index <- final_df$OverallCond*final_df$OverallQual

ggplot(final_df,aes(x = TotalSqft,y =LotArea ))+geom_point(color = "Blue")+ylim(0,50000)+
  ggtitle("Zoomed plot of Total Sqft vs Lot Area")

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



#Jitterplots
p1 <- ggplot(final_df,aes(x=age,y=OverallQual))+geom_jitter(color = "seagreen4" )
p2 <- ggplot(final_df,aes(x=age,y=OverallCond))+geom_jitter(color = "deepskyblue4" )
p3 <- ggplot(final_df,aes(x=age,y=TotRmsAbvGrd))+geom_jitter(color = "coral1" )
grid.arrange(p1,p2,p3,nrow = 2,ncol = 2)


#Neighbood and Sqft
p1 <- ggplot(final_df,aes(x=Neighborhood,y=TotalSqft))+geom_boxplot(fill = "thistle3")
p1 + theme(axis.text.x = element_text(angle = 90)) +ggtitle("Total Sqft in Each Neighborhood")

#Subclass and TotalSqft
p1 <- ggplot(final_df,aes(x = SubClass,y = TotalSqft))+geom_boxplot(fill = "maroon4")
p1

final_df[final_df[,"Neighborhood"]=="Landmrk",]

#EDA with response variable
#TotalSqft, Neighborhood, age, qual_index

#Totalsqft and SalePrice
final_df$logSalePrice <- log(final_df$SalePrice)
p1 <- ggplot(final_df,aes(x=TotalSqft,y = SalePrice))+geom_point(color = "skyblue3")
p2 <- ggplot(final_df,aes(x=TotalSqft,y = logSalePrice))+geom_point(color = "rosybrown3")
grid.arrange(p1,p2,top = "TotalSqft")

#Neighbourhood and Sale Price
p1 <- ggplot(final_df,aes(x=Neighborhood,y = SalePrice))+geom_boxplot(fill = "skyblue3")+
  theme(axis.text.x = element_text(angle = 90))
p2 <- ggplot(final_df,aes(x=Neighborhood,y = logSalePrice))+geom_boxplot(fill = "rosybrown3")+
  theme(axis.text.x = element_text(angle = 90))
grid.arrange(p1,p2,top = "Neighborhood")

#Age and Sale Price
p1 <- ggplot(final_df,aes(x=age,y = SalePrice))+geom_point(color = "skyblue3")
p2 <- ggplot(final_df,aes(x=age,y = logSalePrice))+geom_point(color = "rosybrown3")
grid.arrange(p1,p2,top = "Age of the property")

#qual_index and Sale Price
final_df$qual_index <- as.factor(final_df$qual_index)
p1 <- ggplot(final_df,aes(x=qual_index,y = SalePrice))+geom_boxplot(fill = "skyblue3")
p2 <- ggplot(final_df,aes(x=qual_index,y = logSalePrice))+geom_boxplot(fill = "rosybrown3")
grid.arrange(p1,p2,top = "Quality Index")

#
subdat <- final_df[,c("TotalSqft","age","logSalePrice")]

mcor <- cor(subdat)
library(reshape2)
melted_cormat <- melt(mcor)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low = "steelblue", high = "indianred", mid = "white",limit = c(-1,1))+
  ggtitle("Heatmap")


#Computational Assignment 2
#PART 2
#Based on your EDA from Modeling Assignment #1, 
#focus on 10 of the continuous quantitative variables that you though/think 
#might be good explanatory variables for SALESPRICE.   Is there a way to logically 
#group those variables into 2 or more sets of explanatory variables?   For example,
#some variables might be strictly about size while others might be about quality.  
#Separate the 10 explanatory variables into at least 2 sets of variables. 
#Describe why you created this separation.  A set must contain at least 2 variables.
library("car")
dim(final_df)
#LotArea, TotalBsmtSF, GrLivArea,PoolArea , FirePlaces, GarageArea, WoodDeckSF,
#OpenPorchSF, TotalSqft, qual_index

#Can combine  PoolArea ,WoodDeckSF and OpenPorchSF, GarageArea in one
#Can combine TotalBsmtSF,GrLivArea, TotalSqft in another
#Can combine qual_index, LotArea into third

model3 <- lm(SalePrice ~ TotalBsmtSF + GrLivArea + TotalSqft + Fireplaces, data=final_df)
summary(model3)
coef(model3)
confint(model3)
anova(model3)


residualPlots(model3)
dim(final_df)
#Model 4
model4 <- lm(SalePrice ~ TotalBsmtSF + GrLivArea + TotalSqft + Fireplaces + PoolArea + WoodDeckSF + 
               OpenPorchSF + GarageArea, data = final_df)

summary(model4)
coef(model4)
confint(model4)
anova(model4)

residualPlots(model4)

