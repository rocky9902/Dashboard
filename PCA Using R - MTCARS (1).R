library(dplyr)
data(mtcars)
str(mtcars)
mtcars
mtcars <- mtcars %>% select(- c(vs, am))

mtcars1<-mtcars[,c(mpg,cyl,disp,hp,drat,wt,qsec,gear,carb)]
str(mtcars)
pca <- prcomp(mtcars, center = TRUE,scale. = TRUE)

summary(pca)
pca$sdev ^ 2

print(pca$rotation)
pca %>% biplot(cex = .5)

fit_1 <- lm(mpg ~ ., data = mtcars)

components <- cbind(mpg = mtcars[, "mpg"], pca$x[, 1:2]) %>%
  as.data.frame()

fit_2 <- lm(mpg ~ ., data = components)

summary(fit_1)$adj.r.squared
summary(fit_2)$adj.r.squared
