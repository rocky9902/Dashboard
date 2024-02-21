setwd("D:/MBA/Term 4/MLA-1/Datasets")
getwd()

loan=read.csv("D:/MBA/Term 4/MLA-1/Datasets/default.csv")

summary(loan)
str(loan)

#define dummy variables
loan$Default<-ifelse(loan$Status=="Default",1,0)
loan$female<-ifelse(loan$Gender=="female",1,0)
loan$Management<-ifelse(loan$Job=="Management",1,0)
loan$skilled<-ifelse(loan$Job=="skilled",1,0)
loan$unskilled<-ifelse(loan$Job=="unskilled",1,0)
loan$Poor<-ifelse(loan$Credit.History=="Poor",1,0)
loan$CH.critical<-ifelse(loan$Credit.History=="critical",1,0)
loan$CH.good<-ifelse(loan$Credit.History=="good",1,0)
loan$Ch.verygood<-ifelse(loan$Credit.History=="very good",1,0)
loan$Purpose.car<-ifelse(loan$Purpose=="car",1,0)
loan$Purpose.cd<-ifelse(loan$Purpose=="consumer.durable",1,0)
loan$Purpose.education<-ifelse(loan$Purpose=="education",1,0)
loan$Purpose.personal<-ifelse(loan$Purpose=="personal",1,0)

#scatter plots and correlation(can see multiple collinearity)
loan.Scatter<-subset(loan[,c(2,4:6,9,11:24)])
cor(loan.Scatter)
library(DataExplorer)
plot_correlation(loan.Scatter)


install.packages("corrplot")
library(corrplot)
correlations<-cor(loan.Scatter)
corrplot(correlations, method="number")

#variable selection
#does borrowing purpose explain

boxplot(loan$EMI.Ratio ~ loan$Purpose)
aov.Purpose<-aov(loan$EMI.Ratio~loan$Purpose)
summary(aov.Purpose)
tk.Purpose<-TukeyHSD(aov.Purpose)
tk.Purpose

#variables...does job type explain
boxplot(loan$EMI.Ratio~loan$Job)
names(loan)
aov.Job<-aov(loan$EMI.Ratio~loan$Job)
summary(aov.Job)
tk.Job<-TukeyHSD(aov.Job)
tk.Job

#partioning datasets
#partion of training and testing 
set.seed(1234)
pd<-sample(2,nrow(loan),replace=TRUE, prob=c(0,7,0,3))
train<-loan[pd==1,]
val<-loan[pd==2,]
sum(loan$Default)
sum(train$default)
val$default
View(val)









```{r}
library(caret)

tab.logit<-confusionMatrix(cs_testing$Attrition,pred.plot.logit1,threshold = 0.5)

tab.logit
accuracy.logit<-roc.logit<-roc(cs_testing$No,pred.plot.logit1 )
roc.logit
plot(roc.logit)
```

  