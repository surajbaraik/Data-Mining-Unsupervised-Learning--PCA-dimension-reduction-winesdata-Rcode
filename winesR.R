


# Loading wine data
mydata<-read.csv(file.choose())
View(mydata)
summary(mydata)


## the first column in mydata has wine types
View(mydata[-1]) 
# mydata[-1] -> Considering only numerical values for applying PCA
wine_data <- mydata[,-1]
attach(wine_data)
cor(wine_data)



### PCA suggested CLustering with first three PCs###

# cor = TRUE use correlation matrix for getting PCA scores
pcaObj<-princomp(wine_data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # first 3 PCA Scores which we have to select

# cbind used to bind the data in column wise
# Considering first 3 principal component scores and binding them with wine_data
winepc_data<-cbind(wine_data,pcaObj$scores[,1:3])
View(winepc_data)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-winepc_data[,14:16]

## Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance


## scree plot for selection of no.of clusters with suggested pcs
twss <- NULL
for (i in 2:14){
  twss <- c(twss, kmeans(norm_clus,i)$tot.withinss)
}

plot(2:14, twss, type="b", xlab = "Number of Clusters", ylab = "within groups sum of squares", main ="screeplot")
#from scree plot first elbow bend is at 7 so we can say that we have to go with 7 no. of clusters


## Clustering the data using hclust function --> Hierarchical  with PC suggested
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

plot(fit1, hang=-1)

rect.hclust(fit1, k=7, border="red")

groups<-cutree(fit1,7) # Cutting the dendrogram for 7 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,winepc_data) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wine data on membership_1


library(kselection)
library(doParallel)

##Kmeans clustering with PC
# choosing best clusters as 7 
k_7 <- kmeans(norm_clus,7)
str(k_7)
View(k_7)
winepc_data$cluster <- as.matrix(k_7$cluster)
View(winepc_data)
aggregate(winepc_data,by=list(winepc_data$cluster),mean)

# by forming 7 clusters value of total withiness is reduced and betweeness is increased but we will check that once by elbow turn at 7
# considering 7 clusters is best




### All variable clustering ###

## Normalizing the data 
wine_norm<-scale(wine_data) # Scale function is used to normalize data
dist2<-dist(wine_norm,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

## scree plot for selection of no.of clusters with all variables
twss_2 <- NULL
for (i in 2:14){
  twss_2 <- c(twss_2, kmeans(wine_norm,i)$tot.withinss)
}

plot(2:14, twss_2, type="b", xlab = "Number of Clusters", ylab = "within groups sum of squares", main ="screeplot")
#from scree plot first elbow bend is at 5 so we can say that we have to go with 5 no. of clusters


## Clustering the data using hclust function --> Hierarchical  with all variables
fit2<-hclust(dist2,method="complete") # method here is complete linkage

plot(fit2) # Displaying Dendrogram

plot(fit2, hang=-1)

rect.hclust(fit2, k=5, border="red")

groups2<-cutree(fit2,5) # Cutting the dendrogram for 5 clusters

membership_2<-as.matrix(groups2) # cluster numbering 

View(membership_2)

final2<-cbind(membership_2,wine_data) # binding column wise with orginal data
View(final2)
View(aggregate(final2[,-c(2,9:11)],by=list(membership_2),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wine data on membership_2

##Kmeans clustering with all variables
# choosing best clusters as 5
k_5all <- kmeans(wine_norm,5)
str(k_5all)
View(k_5all)
wine_data$cluster <- as.matrix(k_5all$cluster)
View(wine_data)
aggregate(wine_data,by=list(wine_data$cluster),mean)

# considering 5 clusters is best by taking all variables as that of pc because first three pc we have selected as it is given in problem
#first 3 pc cover only 66% of information so this variation of cluster selection we are getting.

