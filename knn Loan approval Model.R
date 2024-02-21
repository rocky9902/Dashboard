library(class)
file_url <- "https://raw.githubusercontent.com/amarnathbose/Datafiles/master/german_credit.csv"
loan <- read.csv(file_url)
head(4)

loan.subset <- loan[c('Creditability','Age..years.','Sex...Marital.Status','Occupation','Account.Balance','Credit.Amount','Length.of.current.employment','Purpose')]

str(loan.subset)

#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

loan.subset.n <- as.data.frame(lapply(loan.subset[,2:8], normalize))


set.seed(123)
dat.d <- sample(1:nrow(loan.subset.n),size=nrow(loan.subset.n)*0.7,replace = FALSE) #random selection of 70% data.

train.loan <- loan.subset[dat.d,] # 70% training data
test.loan <- loan.subset[-dat.d,] # remaining 30% test data

#Creating seperate dataframe for 'Creditability' feature which is our target.
train.loan_labels <- loan.subset[dat.d,1]
test.loan_labels <-loan.subset[-dat.d,1]

#Install class package
install.packages('class')
# Load class package
library(class)


#Find the number of observation
NROW(train.loan_labels) 

knn.26 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=26)
knn.27 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=27)


#Calculate the proportion of correct classification for k = 26, 27
ACC.26 <- 100 * sum(test.loan_labels == knn.26)/NROW(test.loan_labels)
ACC.27 <- 100 * sum(test.loan_labels == knn.27)/NROW(test.loan_labels)


# Check prediction against actual value in tabular form for k=26
table(knn.26 ,test.loan_labels)

confusion= table(test[, 20],pred)
confusion


acuracy <- sum(diag(confusion)) / sum(confusion)
acuracy


install.packages('caret')
library(caret)

confusionMatrix(table(knn.26 ,test.loan_labels))

