library(DataExplorer)
setwd("D:\\MBA\\Term 4\\MLA-1\\Datasets\\")
getwd()
teledata <- read.csv(
  "Telecom_dummycodding.csv",
  header=TRUE
)
summary(teledata)
str(teledata)
names(teledata)

teledata$churn_factor <- as.factor(teledata$churn_factor)
teledata$International.plan <- as.factor(teledata$International.plan)
teledata$Voice.mail.plan <- as.factor(teledata$Voice.mail.plan)
str(teledata)

teledata= subset(teledata, select = -c(20) )
str(teledata)

#EDA
#HISTORAM
hist(teledata$Customer.service.calls, 
     col="red",
     border="black",
     xlab = "Chrun",
     main = "Histogram")


#BOXPLOT
library(ggplot2)
ggplot2::ggplot(teledata, ggplot2::aes(x=churn_factor, y=Customer.service.calls, fill=churn_factor)) +
  ggplot2::geom_boxplot()

#CORRELATION
library(DataExplorer)           
plot_correlation(teledata)

#BOXPLOT
boxplot(teledata)


library(caTools)
library(randomForest)


# Splitting data in train and test data
split <- sample.split(teledata, SplitRatio = 0.7)
split

train <- subset(teledata, split == "TRUE")
test <- subset(teledata, split == "FALSE")


#MODEL BUILDING USING VARIABLES SELECTED FROM STEP AIC
rf <-randomForest(churn_factor~International.plan + Voice.mail.plan + Number.vmail.messages + 
                    Total.day.minutes + Total.eve.calls + Total.eve.charge + 
                    Total.night.charge,data=teledata,ntree=300)
rf

# Predicting the Test set results
pred = predict(rf, newdata = test[,-20])
pred

# Confusion Matrix
confusion= table(test[, 20],pred)
confusion

#accuracy
acuracy <- sum(diag(confusion)) / sum(confusion)
acuracy


# Plotting model
plot(rf)
plot

# Importance plot
importance(rf)

# Variable importance plot
varImpPlot(rf)




