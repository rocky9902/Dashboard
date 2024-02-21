library(dplyr)
library(data.table)
library(datasets)
library(ggplot2)
setwd("D:\\MBA\\Term 5\\MLA-2\\")
getwd()
data <- read.csv("pizza.csv",header=TRUE)
dim(data)
head(data)
str(data)

pizzas <- copy(data)
pizzas <- pizzas[, brand := NULL]
pca <- prcomp(pizzas, scale. = TRUE)
pca_1_2 <- data.frame(pca$x[, 1:2])

plot(pca$x[,1], pca$x[,2])

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)


ggplot(data,aes(x=data$ash, y=data$fat, color=data$brand))+geom_point()+labs(title = "Pizza brands by two variables")

