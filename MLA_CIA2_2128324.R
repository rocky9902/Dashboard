#Get libraries
library(psych)
library(lmtest)
library(dplyr)
library(factoextra)
library(hopkins)
library(cluster)

#-----------------------------------------------------------------------

#Get data
setwd("D:\\MBA\\Term 5\\MLA-2")
getwd()
df<- read.csv("Cuisines.csv",header=TRUE)
str(df)
#-----------------------------------------------------------------------

#Removing coulmns
d<- df %>% select(- c(Name, Type, Cuisine,Veg,Non.veg))
str(d)

#converting to numeric
d$Cook.time.min.<-as.numeric(d$Cook.time.min.)
d$Total.Fat<-as.numeric(d$Total.Fat)
d$Saturated.Fat<-as.numeric(d$Saturated.Fat)
d$Cholesterol<-as.numeric(d$Cholesterol)
d$Sodium<-as.numeric(d$Sodium)
d$Total.Carbohydrate<-as.numeric(d$Total.Carbohydrate)
d$Dietary.Fiber<-as.numeric(d$Dietary.Fiber)
d$Total.Sugars<-as.numeric(d$Total.Sugars)
d$Protein<-as.numeric(d$Protein)
d$Vitamin.C<-as.numeric(d$Vitamin.C)
d$Calcium<-as.numeric(d$Calcium)
d$Iron<-as.numeric(d$Iron)
d$Potassium<-as.numeric(d$Potassium)
d$Calories<-as.numeric(d$Calories)
str(d)
#----------------------------------------------------------------------

#PCA
pca <- prcomp(d, center = TRUE,scale. = TRUE)
summary(pca)

# Visusalisation
fviz_eig(pca)  # eigenvalues on y-axis
fviz_pca_var(pca, col.var="steelblue")
#-------------------------------------------------------------------------

#In order to determine the adequate number of PCs, two methods will be used: 
#Kaiser Criterion and Scree Plot results with parallel analysis.

#Kaiser Criterion
eig.val<-get_eigenvalue(pca)
eig.val

#Parallel analysis with Scree Plot
library(paran)
paran(d, iterations=10000, quietly=FALSE,
      status=FALSE, all=TRUE, cfa=FALSE, graph=TRUE,
      color=TRUE, col=c("black","red","blue"),
      lty=c(1,2,3), lwd=1, legend=TRUE, file="",
      width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)
#--------------------------------------------------------------------------

#Now, let's see which variables constitute the most for six principal
#components (which carry the most variance).
#PC1
var<-get_pca_var(pca)
a<-fviz_contrib(pca, "var", axes=1, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of first Principal Components")

#PC2
var<-get_pca_var(pca)
a<-fviz_contrib(pca, "var", axes=2, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of second Principal Components")

#PC3
var<-get_pca_var(pca)
a<-fviz_contrib(pca, "var", axes=3, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of third Principal Components")

#PC4
var<-get_pca_var(pca)
a<-fviz_contrib(pca, "var", axes=4, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of fourth Principal Components")

#PC5
var<-get_pca_var(pca)
a<-fviz_contrib(pca, "var", axes=5, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of fifth Principal Components")

#PC6
var<-get_pca_var(pca)
a<-fviz_contrib(pca, "var", axes=6, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of sixth Principal Components")
#-------------------------------------------------------------------------------

#Clustering tendency - Hopkins statistic
pca<-prcomp(d, center=TRUE, scale.=TRUE, rank. = 6)
results <- pca$x
get_clust_tendency(results, (n=nrow(results)-1)) #value is closer to 1, data clusterable
#---------------------------------------------------------------------------------

#Silhouette Statistic
results=cbind(d$Veg,d$Non.veg,results)|> as.data.frame()
fviz_nbclust(results, FUNcluster=kmeans, k.max = 8) 
#-------------------------------------------------------------------------------
#K-Means Clustering

km1<-eclust(results, "kmeans", hc_metric="gowers",k=2)
fviz_cluster(km1, data = results)

fviz_silhouette(km1) 

#find means of each cluster
aggregate(d, by=list(cluster=km1$cluster), mean)

#add cluster assigment to original data
final_km <- cbind(df, cluster = km1$cluster)

#view final data
View(final_km)

write.csv(final_km,"final_km.csv")

#--------------------------------------------------------------------------

#Hierarchical clustering
dm<-dist(results) 
hc<-hclust(dm, method="ward.D2")
plot(hc)
rect.hclust(hc, k=2, border="red") 

# Cutting tree by no. of clusters
fit <- cutree(hc, k = 2)
final_hc <- cbind(df, cluster = fit)
View(final_hc)

#writing a csv file
write.csv(final_km,"final_hc.csv")

#-----------------------------------------------------------------------

