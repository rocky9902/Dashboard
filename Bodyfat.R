#MLA_CIA_1
#ROSHAN RAVINDRA_2128324

#IMPORTING THE LIBRARIES
library(lmtest)
library(MASS)
library(car)
library(DataExplorer)
library(gvlma)
library(Amelia)
library(ggplot2)
library(markdown)
library(rmarkdown)
library(knitr)
library(psych)
library(DataExplorer)
library(lmtest)
library(MASS)
library(car)
library(DataExplorer)
library(gvlma)
library(Amelia)
library(Metrics)
library(car)
library(qicharts)
library(lawstat)
library(readxl)

#LOADING THE DATASET
a<-read.csv("D:/MBA/Term 4/MLA-1/Datasets/bodyfat.csv")

#DATA EXPLORATION
summary(a)
str(a)
names(a)
nrow(cs)
ncol(cs)

#HISTORAM
hist(a$Age, # histogram
     col="red", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Age",
     main = "Histogram")

#DENSITY PLOT
plot_density(a)

#SCATTER PLOT
library(car)
pairs(~BodyFat + Density + Weight + Height, data = b, main = "Scatterplot Matrix")

#CORRELATION
library(DataExplorer)           
plot_correlation(a)

#BOXPLOT
boxplot(a)

#MISSING VALUE ANALYSIS
plot_missing(a)
AmeliaView()
b<-read.csv("D:/MBA/Term 4/MLA-1/Datasets/body_fat.csv")
plot_missing(b)

#FEATURE ENGINEERING
b$BF_Level<-ifelse(b$BodyFat>19 ,1,0)
str(b)

#MODELLING
#MODEL SELECTION AND ASSUMPTIONS
#CORRELATION
library(DataExplorer)
plot_correlation(b)

#HISTOGRAM
library(Hmisc)
hist.data.frame(b)

#SCATTER PLOT
library(car)
scatterplotMatrix(b)


#MODEL OUTPUT
set.seed(1234)
b_mixed<-b[order(runif(252)),]
b_training<-b_mixed[1:176,]
b_testing<-b_mixed[177:252,]
m<-lm(BodyFat ~.,data = b_training)
summary(m)

#STEPWISE MODEL BUILDING
library(MASS)
step_m<-stepAIC(m,direction = "backward")

#MULTICOLLINEARITY
vif(m)


#MODEL FITTING
n<-lm(BodyFat ~ Density + Age + Weight + Ankle,data = b_training)
vif(n)
summary(n)
anova(n)

#RESIDUAL ANALYSIS
plot(n)


#PREDICTION FOR THE TEST DATA
p<-predict(n,b_testing)
p

#Confidence Interval of 95%
confint(n, level=0.95)


#MODEL EVALUATION AND DIAGNOSTICS
#diagnosis
plot(n)

library(Metrics)
#MEAN SQUARED ERROR
mse<- mse(b_testing$BodyFat,p)
mse

#RESIDUAL STANDARD ERROR
rse<- rse(b_testing$BodyFat,p)
rse


#EVALUATION
#NORMALITY
qqPlot(n)

#LINEARITY
crPlots(n)

#AUTOCORRELATION
library(car)
durbinWatsonTest(n)

#NON-CONSTANT VARIANCE or BREWSCH-PAGAN TEST
library(lmtest)
bptest(n)






