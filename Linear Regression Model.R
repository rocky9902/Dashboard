p=read.csv("D:/MBA/Term 4/MLA-1/Datasets/Hotel.csv")
print(p)
 #correlation
library(car)
library(DataExplorer)
library(psych)
library(readxl)
#Correlation
cor(p)
#Scatter plot to check linearity
scatter.smooth(p)
#Boxplot for outliers
boxplot(p)
#Density plot for normal distribution
density(p)


fullmodel_cp<-lm(Revenueindollar~.,data = Hotel)
summary(fullmodel_cp)
