library(cluster)
library(factoextra)
wine <- read.csv(file.choose())
View(wine)
help("princomp")
View(wine[-1])
data <- wine[,-1]
attach(data)
cor(data)

#cor = TRUE use correlation matrix for getting pca score
pcaObj <- princomp(data, cor=TRUE, scores = TRUE, covnat = NULL)
str(pcaObj)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of princomp
biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type = "b")
pcaObj$scores[,1:3]




# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata

wine <- cbind(mydata,pcaObj$scores[,1:3])
View(wine)

#hierarchial clustering
# preparing data for clustering
clus_data <- wine[,8:10]

#Normalizing thw data

norm_clus <- scale(clus_data)
dist2 <- dist(norm_clus,method = "euclidean")
#considering a euclidean

#clustering the data using hclust function --> Hierarchical
fit2 <- hclust(dist2,method = "complete")
plot(fit2)
rect.hclust(fit2, k=7, border = "yellow")

groups<-cutree(fit2,5) # Cutting the dendrogram for 5 clusters
membership_2<-as.matrix(groups) # cluster numbering 
View(membership_2)
final2<-cbind(membership_2,wine) # binding column wise with orginal datView(final)
View(aggregate(final2[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be

# drawn from the aggregate of the universities data on membership_1
write.csv(final2,file="wine_cluster.csv",row.names = F,col.names = F)
getwd()


#k-mean
library(plyr)
?plyr

wine <- read.csv(file.choose())
str(wine)


View(wine)

normalized_data <- scale(wine[,1:9])
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final3 <- data.frame(fit$cluster,wine)
View(final3)
aggregate(wine[,1:9], by=list(fit$cluster),FUN=mean)
table(fit$cluster)
