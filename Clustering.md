# 3. Cluster Analysis
# 3.1 Hierchical-single
```r
mydata.s <- scale(clean_data)
dist <- dist(mydata.s) #distance matrix calculation
hc1 <- hclust(dist, "single")
plot(hc1, hang=-1, cex=0.5, main ="Single Linkage HC Dendogram") #making a dendogram plot

#scree plot
plot(rev(hc1$height), xlim=c(1,20)) 
ct<- cutree(hc1, 2)
table(ct)
pca <- princomp(mydata.s)
plot(pca$scores[, 1:2], col=ct) 
plot(pca$scores[, 2:3], col=ct) 
plot(pca$scores[, c(1,3)], col=ct)
```
## Cluster Analysis

Cluster analysis enabled us to divide countries with similar happiness characteristics into three groups: North America/Western Europe/Australia, Latin America/North Africa/Northeast Asia, and Southern Asia/Africa. Our analysis showed that model-based clustering was the most effective technique for this data because it was multivariate normal and we were able to interpret the clusters using a plot of PC1 and PC2.

## Hierarchical-Single

Hierarchical clustering with single linkage did not produce clearly defined clusters through the dendrogram, scree plot, and principle component plots. The scree plot suggests the presence of two clusters, but the principle component plot appears to show only one observation in the second group.

# 3.2 Hierarchical-complete
```r
dist <- dist(mydata.s) #create a distance matrix
hc1 <- hclust(dist, "complete")
plot(hc1, hang=-1, cex=0.5, main ="Complete Linkage HC Dendogram") #ploting the dendogram
#scree plot
plot(rev(hc1$height),xlim=c(1,20)) 
ct<- cutree(hc1, 3) 
table(ct)
pca <- princomp(mydata.s) 
plot(pca$scores[, 1:2], col=ct)
plot(pca$scores[, 2:3], col=ct) 
plot(pca$scores[, c(1,3)], col=ct)
```
## Hierarchical-Complete
Hierarchical clustering with complete linkage provided clearer groupings than single linkage hierarchical clustering, but the groups were still quite vague. The scree plot showed two clusters, but the principle component plot showed overlap and it was not possible to assign clear meanings to the principal components.

# 3.3Hierchical-Average
```r
dist <- dist(mydata.s) #making a distance matrix
hc1 <- hclust(dist, "average")
plot(hc1, hang=-1, cex=0.5, main ="Average Linkage HC Dendogram") #ploting a dendogram
#scree plot
plot(rev(hc1$height),xlim=c(1,20)) 
ct<- cutree(hc1, 3) 
table(ct)
pca <- princomp(mydata.s) 
plot(pca$scores[, 1:2], col=ct)
plot(pca$scores[, 2:3], col=ct) 
plot(pca$scores[, c(1,3)], col=ct)
```

## Hierarchical-Average

Hierarchical clustering with average linkage produced worse results than single linkage and complete linkage hierarchical clustering. None of the clustering techniques yielded clear groups. The scree plot indicated three clusters, but the principle component plot showed that there was only one observation in one of the groups. It was not possible to assign clear meanings to the principal components.

# 3.4 K-Means
```r
km <- kmeans(mydata.s, centers=3, nstart = 10) 
plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc) 
  for (i in 1:maxc)
    wss[i] = kmeans(mydata,centers=i, nstart = 10)$tot.withinss 
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot")
}

plot.wgss(mydata.s, 20)

pca <- princomp(mydata.s) 
plot(pca$scores[, 1:2], col=km$cluster)
plot(pca$scores[, 2:3], col=km$cluster) 
plot(pca$scores[, c(1,3)], col=km$cluster)
```

## K-Means

K-means clustering provided more satisfactory results than hierarchical clustering, but the clustering groups were not as clear as those obtained using model-based clustering. The scree plot showed three clusters, but the principle component plot showed overlap. As a result, it was not possible to assign clear meanings to the principal components.

# 3.5 Model-based(Unsupervised)
```r
library(mclust) 
mc<-Mclust(mydata.s) 
summary(mc)
plot(mc, what="BIC") 
table(mc$classification)
clust.data = cbind(rownames(mydata.s), mc$classification, mc$uncertainty) 
clust.data[order(mc$uncertainty),]
plot(pca$scores[, 1:2], col=mc$classification) 
plot(pca$scores[, 2:3], col=mc$classification) 
plot(pca$scores[, c(1,3)], col=mc$classification)
```
## Model-Based (Unsupervised)

The data that was multivariate normal provided the best clustering groups using model-based clustering techniques. Jordan had the highest uncertainty probability. The model-based clustering identified three groups. The principle component plot showed that two clusters could be described with PC1, while the third cluster could be described with PC2. PC1 represents the overall happiness performance, while PC2 represents countries that performed well in personal happiness-related variables but poorly in societal-related variables.

### 3.5.1 Ploting the above model in a world map
```r
library(maptools) 
data(wrld_simpl)
cluster1=subset(rownames(mydata.s), mc$classification==1) 
cluster2=subset(rownames(mydata.s), mc$classification==2)
cluster3=subset(rownames(mydata.s), mc$classification==3) 
cluster=c(cluster1, cluster2,cluster3)
select_countries = wrld_simpl@data$NAME %in% cluster1 
plot(wrld_simpl, col = c(gray(.50), "orange")[select_countries+1])
select_countries = wrld_simpl@data$NAME %in% cluster2 
plot(wrld_simpl, col = c(gray(.50), "tomato")[select_countries+1])
select_countries = wrld_simpl@data$NAME %in% cluster3 
plot(wrld_simpl, col = c(gray(.50), "violet")[select_countries+1])
```
# 3.6 Model-Based Discriminant Clustering
```r
new_happiness <- happiness_after_outlier 
new_happiness$Group[new_happiness$Score< 4]<-"Low" 
new_happiness$Group[new_happiness$Score>=4 & new_happiness$Score<6 ]<-"Medium"
new_happiness$Group[new_happiness$Score>=6]<-"Higher"
new_happiness =new_happiness[sample(1:149,149),]
data.train = new_happiness[1:110, c(-1, -8)] 
label.train = new_happiness[1:110, 8] 
data.test = new_happiness[111:149, c(-1, -8)] 
label.test = new_happiness[111:149, 8]
DA <- MclustDA(data.train, label.train)
summary(DA, newdata = data.test, newclass = label.test)
```
## Model-Based Discriminant (Supervised)
Since the true cluster was not available in the given dataset, we created group labels based on the
happiness score.

Happiness score             Happiness Rank
Less than 4                     Low
4 – 6                          Medium
6 or more                       High

With random training sample of 110 observations and testing sample of 39 observations, we can
classify group of any new observation with 70% to 90% accuracy.

