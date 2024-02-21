
##Employee Attrition prediction using Decision Tree Algorithm
# import libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(caTools)
library(randomForest)
#Step 1) Import the data

setwd("D:\\MBA\\Term 4\\MLA-1\\Datasets")
getwd()
hr <- read.csv("HR_Dataset.csv", stringsAsFactors = T) %>% 
  mutate(status = as.factor(ifelse(left == 1, "Left", "Stayed")))

hr <- read.csv("HR_Attrition.csv")
dim(hr)
head(hr)
str(hr)
summary(hr)

#Step 2) Clean the dataset

sum(is.na(hr))  #no NA
which(is.na(hr))

#Step 3)  Exploratory Data Analysis                                    

data.frame(Numeric = sapply(hr, is.numeric)) 
data.frame(Role = unique(hr$role))
data.frame(Salary = unique(hr$salary))

# Correlation Heatmap for Numeric Columns

numhr <- hr[, sapply(hr, is.numeric)]
corr <- cor(numhr)
corr[upper.tri(corr)] <- NA

meltcorr <- melt(corr) %>% 
  mutate(Var1 = factor(Var1, levels = rev(levels(.$Var1))), Var2 = factor(Var2))

ggplot(meltcorr, aes(Var1, Var2, fill = value, label = round(value,2))) +
  ggtitle("Correlation Heatmap") + 
  geom_tile() +
  geom_text(size = 3) +
  coord_equal() +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(angle = -90, hjust = 0)) +
  scale_x_discrete(name = "") +
  scale_y_discrete(name = "") +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "navy",
                       midpoint = 0, na.value = "white", 
                       name = "Correlation", limits = c(-1, 1))

# Freq Plot (all discrete features)

aes = as.factor(number_project) #, exp_in_company, work_accident, promotion_last_5years
ggplot(hr, aes(as.factor(number_project))) + geom_bar()

# fill = status, salary, role
ggplot(hr, aes(as.factor(number_project), fill = status)) + 
  geom_bar(position = "stack") +
  scale_fill_hue()

## Freq Plot (all categorical features)

# role, salary, status
ggplot(hr, aes(role)) + geom_bar()

# fill = status
ggplot(hr, aes(role, fill = status)) + 
  geom_bar(position = "stack") +
  scale_fill_hue()

# Distribution Plot (continuous features)

# satisfaction_level, last_evaluation, average_monthly_hours
ggplot(hr, aes(satisfaction_level)) + geom_bar()

# # fill = status, salary
ggplot(hr, aes(last_evaluation, fill = salary)) + 
  geom_bar(position = "stack") +
  scale_fill_hue()


## Boxplot & Violin Plot

# x / fill = role, salary, number_project, exp_in_company, work_accident, promotion_last_5years
# y = satisfaction_level, last_evaluation, average_monthly_hours

ggplot(hr, aes(x = salary, y = satisfaction_level, colour = status)) + 
  geom_boxplot() + scale_fill_hue()

ggplot(hr, aes(x = salary, y = satisfaction_level, fill = status)) + 
  geom_violin() + scale_fill_hue()

ggplot(hr, aes(x = salary, y = satisfaction_level, fill = status)) + 
  geom_violin(position = position_dodge(width = 0.4)) + 
  geom_boxplot(position = position_dodge(width = 0.4), width = 0.1) +
  scale_fill_hue()

remove.packages("r-lib/rlang")
#pak::pkg_uninstall("r-lib/rlang")

#Step 4 Clean the dataset
install.packages("rlang")
library(devtools)
library(caTools)
library(rlang)
library(caret)
set.seed(111)
split_numdata <- sample.split(hr, SplitRatio = .75)

sample = sample.split(hr, SplitRatio = .75)
train = subset(hr, sample == TRUE)
test  = subset(hr, sample == FALSE)
dim(train)
dim(test)

train <- numhr[ split_numdata,]
test <- numhr[-split_numdata,]
dim(train)
dim(test)

#Step 5) Build the model
fit <- rpart(left~., data = train, method = 'class') #recurrsive partioning
rpart.plot(fit, extra = 106) #doubt 106?


#Step 6) Make a prediction
predict_left <-predict(fit, test, type = 'class') 



#Step 7) Measure performance
table_mat <- table(test$left, predict_left)
table_mat

acuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', acuracy_Test))
#[1] "Accuracy for test 0.968266666666667"
#Step 8) Tune the hyper-parameters
#Decision tree in R has various parameters that control aspects of the fit. In rpart decision tree library, you can control the parameters using the rpart.control() function.
#Tune the maximum depth
#Tune the minimum number of sample a node must have before it can split
#Tune the minimum number of sample a leaf node must have

accuracy_tune <- function(fit) {
  predict_left <- predict(fit, test, type = 'class')
  table_mat <- table(test$left, predict_left)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


control <- rpart.control(minsplit = 6,
                         minbucket = round(7 / 5),
                         maxdepth = 5,
                         cp = 0)
tune_fit <- rpart(left~., data = train, method = 'class', control = control)
accuracy_tune(tune_fit)
#[1] 0.9730667

control <- rpart.control(minsplit = 7,
                         minbucket = round(8 / 6),
                         maxdepth = 6,
                         cp = 0)
tune_fit <- rpart(left~., data = train, method = 'class', control = control)
accuracy_tune(tune_fit)
#[1] 0.9752

#comparision with logistic regression
#logistic regression

model <- glm (left ~ ., data = train, family = binomial)
summary(model)


predict <- predict(model, newdata = test, type = 'response')
predict[predict >= 0.5] = 1
predict[predict < 0.5] = 0

test$left <- as.factor(test$left)
predict <- as.factor(predict)
confusionMatrix(test$left, predict)
#Accuracy : 0.7691 




