library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(ggpubr)
library(moments)
library(caret)
library(caTools)
library(Hmisc)
library(lattice)
library(Formula)
library(survival)
library(forecast)
library(corrplot)
library(car)
library(ROCR)
library(Metrics)
library(VIM)
library(rpart)       
library(rpart.plot)  
library(rattle)
library(FNN)
library(car)
###########################################
#1. Loading Data
data <- read.csv("Life Expectancy Data.csv")
head(data)
sprintf("Dataset size: [%s]", toString(dim(data)))

#2. Clean and filter data.
#2.1 Remove unnecessary variables
data <- subset(data, select = -c())

#2.2 Missing data
missing.rows = dim(data)[1] -  dim(na.omit(data))[1]
sprintf("Dataset size: [%s]", toString(dim(data)))
sprintf("Missing rows: %s (%s%%)", missing.rows, round((missing.rows*100)/dim(data)[1], 2))

missings_df <- data.frame(type=c("missing", "non-missing") ,count = c(missing.rows,  dim(na.omit(data))[1]))


ggplot(missings_df, aes(fill=type, y="", x=count)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Missing vs Non-missing row counts") +
  xlab("Missing count") + ylab("") +
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Set1")


missing_counts <- data.frame(feature = factor(names(data)),
                             counts=sapply(data, function(x) sum(is.na(x))))


ggplot(missing_counts,
       aes(x=reorder(feature, -counts), y=counts, fill=counts)) +
  geom_bar(stat="identity") +
  ggtitle("Missing counts in each feature") +
  xlab("Feature") + ylab("Missing count") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_continuous(trans = 'reverse')
#As we have more than 40% of missing data, we can apply data imputation as following:
#Checking outliers in each variable that contains missings using boxplots
#The variables with high outliers will apply imputation with median
#The variables with low outliers will apply imputation with mean.


par(mfrow=c(2,7))
boxplot(data$Life.expectancy,
        ylab = "Life Expectancy",
        main = "Boxplot of Life Expectancy",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Adult.Mortality,
        ylab = "Adult Mortality",
        main = "Boxplot of Adult Mortality",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Alcohol,
        ylab = "Alcohol",
        main = "Boxplot of Alcohol",
        col= "#008080",
        outcol="#008080")
boxplot(data$Hepatitis.B,
        ylab = "Hepatitis B",
        main = "Boxplot of Hepatitis B",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$BMI,
        ylab = "BMI",
        main = "Boxplot of BMI",
        col= "#008080",
        outcol="#008080")
boxplot(data$Polio,
        ylab = "Polio",
        main = "Boxplot of Polio",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Total.expenditure,
        ylab = "Total Expenditure",
        main = "Boxplot of Total Expenditure",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Diphtheria,
        ylab = "Diphteria",
        main = "Boxplot of Diphteria",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$GDP,
        ylab = "GDP",
        main = "Boxplot of GDP",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Population,
        ylab = "Population",
        main = "Boxplot of Population",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$thinness..1.19.years,
        ylab = "Thinness 1-19 years",
        main = "Boxplot of Thinness for 1-19 years old",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$thinness.5.9.years,
        ylab = "Thinness 5-9 years",
        main = "Boxplot of Thinness for 5-9 years old",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Income.composition.of.resources,
        ylab = "Income Composition",
        main = "Boxplot of Income Composition",
        col= "#008080",
        outcol="#008080")
boxplot(data$Schooling,
        ylab = "Schooling",
        main = "Boxplot of Schooling",
        col= "#FF6666",
        outcol="#FF6666")


mydata=kNN(data,variable=c("Alcohol","Hepatitis.B","Polio","Total.expenditure","Diphtheria","Life.expectancy","Adult.Mortality","thinness..1.19.years","Income.composition.of.resources","Schooling","thinness.5.9.years","BMI","Population","GDP"),k=9)
#Removing the unwanted variables created during imputation
mydata1=subset(mydata,select=Country:Schooling)
#Creation of dummy variable
mydata1$Developed=ifelse(mydata$Status=="Developed",1,0)
attach(mydata)
##Univariate analysis 

histogram(mydata$Adult.Mortality,main="Histogram of Adult Mortality",xlab="Adult Mortality")#Normal distribution
histogram(mydata$infant.deaths,main="Histogram of infant.deaths",xlab="infant.deaths")#Skewed at right
histogram(mydata$Alcohol,main="Histogram of Alcohol",xlab="Alcohol")#Skewed at right
histogram(mydata$percentage.expenditure,main="Histogram of percentage.expenditure",xlab="percentage.expenditure" )#Skewed at right
histogram(mydata$Hepatitis.B,main="Histogram of Hepatitis.B",xlab="Hepatitis.B")#Skewed at left
histogram(mydata$Measles,main="Histogram of Measles",xlab="Measles")#Skewed at right
histogram(mydata$BMI,main="Histogram of BMI",xlab="BMI")#Bimodal
histogram(mydata$under.five.deaths,main="Histogram of under five deaths",xlab="under five deaths")
histogram(mydata$Polio,main="Histogram of Polio",xlab="Polio")#Skewed at left
histogram(mydata$Total.expenditure,main="Histogram of Total.expenditure",xlab="Total.expenditure")#Normal distribution
histogram(mydata$Diphtheria,main="Histogram of Diphtheria",xlab="Diphtheria")#Skewed at left
histogram(mydata$HIV.AIDS,main="Histogram of HIV.AIDS",xlab="HIV.AIDS")#Skewed at right
histogram(mydata$thinness..1.19.years,main="Histogram of thinness  1-19 years",xlab="thinness  1-19 years")
#Normal distribution
histogram(mydata$thinness.5.9.years,main="Histogram of thinness 5-9 years",xlab="thinness 5-9 years")#Normal distribution
histogram(mydata$Income.composition.of.resources,main="Histogram of Income.composition.of.resources",xlab="Income.composition.of.resources")#Normal distribution
histogram(mydata$Schooling,main="Histogram of Schooling",xlab="Schooling")#Normal distribution
barplot(mydata1$Developed)

#check for outliers
boxplot(Adult.Mortality~Status)
boxplot(mydata1$infant.deaths~Status)
boxplot(mydata1$Alcohol~Status)
boxplot(mydata1$percentage.expenditure~Status)
boxplot(mydata1$Hepatitis.B~Status)
boxplot(mydata1$Measles~Status)
boxplot(mydata1$BMI~Status)#No outliers
boxplot(mydata1$under.five.deaths~Status)
boxplot(mydata1$Polio~Status)
boxplot(mydata1$Total.expenditure~Status)
boxplot(mydata1$Diphtheria~Status)
boxplot(mydata1$HIV.AIDS~Status)
boxplot(mydata1$GDP~Status)
boxplot(mydata1$Population~Status)
boxplot(mydata1$thinness..1.19.years~Status)
boxplot(mydata1$thinness.5.9.years~Status)
boxplot(mydata$Income.composition.of.resources~Status)
boxplot(mydata1$Schooling~Status)

#Correlation check
corrplot(cor(mydata1[,-c(1:3)]),type="upper",method="circle",title="Correlation plot between variables",mar=c(0.1,0.1,0.1,0.1))

#Adult.Mortality, thinness 1-19 years, thinness 5-9 years , HIV.AIDS are negatively correlated with Life expectancy.And there is positive correlation of Income.composition.of.resources,Schooling, percentage expenditure, Polio, Diphtheria ,developed and BMI with Life expectancy.
#The variable infant death and under five death are highly correlated
#There is high correlation between thinness..1.19.years and thinness.5.9.years
#GDP and percent expenditure are highly correlated.
#There is multicollinearity present because there is correlation between independent variables
#Insights from EDA:-

#Life expectancy increased across years.
#In developed countries adult mortality rate,prevelance of thinness of child between 1 to 19 years,infants deaths and deaths from HIV.AIDS are less comparing to developing countries which ultimately leads higher life expectancy in developed countries.
#On the other hand the other variables such as Income composition of resources ,number of years of schooling,percentage expenditure and Total expenditure on health are more in developed countries than developing countries which again increase the life expectancy in developed countries.
#Also by the increased numbers of immunization coverage against Hepatitis B, Polio and Diphtheria in developed countries results to high life expectancy in developed countries in contrast to developing countries.
#Life expectancy is high in developed countries comparing to developing countries.

#Stepwise removal of variables 

vif(lm(Life.expectancy~.-Country-Year-Status-Life.expectancy,data=mydata1))
vif(lm(Life.expectancy~.-Country-Year-Status-Life.expectancy- infant.deaths - thinness.5.9.years-GDP,data=mydata1))

#The variables such as infant.deaths,GDP and thinness..5.9.years with high VIF(>5) are removed from the final model.
#final data after removing the unwanted variable
finaldata=mydata1[,-c(3,6,17,20)]
#Data Partition 
train=subset(finaldata, Year <= 2013)
test=subset(finaldata, Year >= 2014)
dim(train)
dim(test)
#Scaling the data
scale = preProcess(train[,-c(1,2)], method = "range")
scaled.train = predict(scale, train[,-c(1,2)])
scale = preProcess(test[,-c(1,2)], method = "range")
scaled.test = predict(scale, test[,-c(1,2)])
#Data is split into train and test data based on the variable year train data -2000 to 2013 test data - 2014 to 2015
#Data scaling is done for all the numeric variables


###Linear regression model
train.LR=scaled.train
test.LR=scaled.test
#Base model with all variables
model1=lm(Life.expectancy~.,data=train.LR) 
summary(model1)
#final model
model2=lm(Life.expectancy~.-Population-Hepatitis.B-Alcohol-Total.expenditure-under.five.deaths,data=train.LR) 
summary(model2)
#Alcohol,Total.expenditure,Hepatitis.B,Population,thinness..1.19.years are not significant,so removed from the final model.Top 5 variable effecting Life expectancy are Adult.Mortality,BMI,HIV.AIDS,Schooling and Income.composition.of.resources.
#Prediction using train data 
train.pred=train.LR
train.pred$predTrain=predict(model2, newdata = train.pred,type = "response")
#Mean Absolute ERROR
mae(train.pred$Life.expectancy,train.pred$predTrain)

#ROOT MEAN SQUARE ERROR
rmse(train.pred$Life.expectancy,train.pred$predTrain)


#Prediction using test data 
test.pred=test.LR
test.pred$predTest=predict(model2, newdata = test.pred,type = "response")

#Mean Absolute ERROR
mae(test.pred$Life.expectancy,test.pred$predTest)


#ROOT MEAN SQUARE ERROR
rmse(test.pred$Life.expectancy,test.pred$predTest)

#Linear regression

#The mean absolute error and root mean square error for train and test data is 0.058 and 0.078.
#The mean absolute error and root mean square error for test data is 0.064 and 0.087
###Regression tree
train.cart=scaled.train
test.cart=scaled.test
#First tree
m1 <- rpart(
  formula = Life.expectancy ~ .,
  data    = train.cart,
  method  = "anova", 
  control = list(cp = 0, xval = 10,minsplit=60, minbucket = 20)
)
plotcp(m1)
abline(v = 15, lty = "dashed")
#Final tree
m2 <- rpart(formula = Life.expectancy ~ .,data=train.cart,method= "anova",cp=0.003,minsplit=60, minbucket = 20)
m2
rpart.rules(m2,style = "tallw", cover = FALSE, nn = FALSE,
            roundint = TRUE, clip.facs = FALSE,
            varorder = NULL)
#plotting the decision tree
fancyRpartPlot(m2)
##Identify the importance of the variables
m2$variable.importance
#Prediction using train data 
train.pred.cart=train.cart
train.pred.cart$predTrain.cart=predict(m2, newdata = train.pred.cart)
#Mean Absolute ERROR
mae(train.pred.cart$Life.expectancy,train.pred.cart$predTrain.cart)
#ROOT MEAN SQUARE ERROR
rmse(train.pred.cart$Life.expectancy,train.pred.cart$predTrain.cart)

#Prediction using test data 
test.pred.cart=test.cart
test.pred.cart$predTest.cart=predict(m2, newdata = test.pred.cart)

#Mean Absolute ERROR
mae(test.pred.cart$Life.expectancy,test.pred.cart$predTest.cart)

#ROOT MEAN SQUARE ERROR
rmse(test.pred.cart$Life.expectancy,test.pred.cart$predTest.cart)

#Regression tree

#The mean absolute error and root mean square error for train and test data is 0.043 and 0.057.
#The mean absolute error and root mean square error for test data is 0.055 and 0.076

##KNN Regression
train.knn=scaled.train
test.knn=scaled.test

trControl=trainControl(method='repeatedcv',number=10,repeats=3)
set.seed(100)
fit=train(Life.expectancy~.,data=train.knn,tuneGrid=expand.grid(k=1:20),method='knn',trControl=trControl)
varImp(fit)
#prediction using KNN
pred=predict(fit,newdata=test.knn)
#Mean Absolute ERROR
mae(test.knn$Life.expectancy,pred)
#ROOT MEAN SQUARE ERROR
rmse(test.knn$Life.expectancy,pred)
#KNN regression

#The mean absolute error and root mean square error for test data is 0.061 and 0.076
#Based on performance measures among all models regression Tree shows least Mean absolute error and Root mean square error.So Regression tree is considered as best model to predict Life expectancy.