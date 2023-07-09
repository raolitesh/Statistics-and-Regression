# 2.Data Cleaning & Visualization
For the purpose of our multivariate analysis, we analyzed six variables that have an impact on the overall happiness score.
# 2.1 Correlation and Missing Values
## Missing Values
In this study, values of zero were treated as missing values and replaced with the mean of the column. With this process, our data does not have any missing values and we can now proceed to detecting and removing outliers.
```r
happiness<- happiness_2019[, c(-1)] # Removing Overall.Rank for the purpose of analysis as it doesn't represent important values
#let's keep Score for now
head(happiness) # see the new dataframe after removing Overall.Rank
happiness[happiness == 0] <- NA #converting any values in our dataframe that contains 0 into NA for better analysis
head(happiness)
for(i in 1:ncol(happiness)){
  happiness[is.na(happiness[,i]), i] <- mean(happiness[,i], na.rm = TRUE) 
  #if there are any NA values in a particular column attribute
  #I filled them with mean values of that column
}
```
```r
library(corrplot)
newdata <-  cor(happiness[-1]) 
head(newdata)
corrplot(newdata, method = "number") #used the method in the correlation plot as numbers to better visualize the correlation between attributes
```
The correlation between GDP per capita, Social support, and Health life expectancy and Generosity is low. However, the correlation between GDP per capita, Health life expectancy, and Social support is high. The goal of dimension reduction analysis is to reduce the number of variables and represent these highly correlated variables with a smaller number of transformed variables.

# 2.2 Multivariate normality test for Outlier Detection
```r
x_val <- colMeans(happiness[-1])
test <- cov(happiness[-1])
val <- mahalanobis(happiness[-1], x_val, test) 
sort_val <-sort(val)
quantiles <-qchisq((1:nrow(happiness[-1])-1/2)/nrow(happiness[-1]), df=ncol(happiness[-1]))
plot(quantiles, sort_val,
     xlab=expression(paste(chi[3]^2,"Quantile")),
     ylab="Ordered squared distances", main="") 
abline(a=0, b=1)
text(quantiles, sort_val, abbreviate(names(sort_val)), col="blue", pch=0.8)
#seven outlier countries matched with the data-frame that are far away from the multivariate normality
outlier_country <- match(lab <- c("Syria", "Somalia", "Rwanda", "Haiti", "Myanmar", "Indonesia", "Central African Republic"), row.names(happiness))
happiness_after_outlier <- happiness[-outlier_country,]
happiness_after_outlier
clean_data <- happiness_after_outlier[-1] 
str(clean_data)
#now we can drop the happiness score
```
We see a clear deviation from multivariate normality. However, those that are far from the line be treated as outliers. "Syria", "Somaliland region", "Rwanda", "Haiti", "Myanmar", "Indonesia", "Central African Republic" are outliers and we removed these observations from the data.

# 2.3 Multivariate normality test with clean data
```r
x_val <- colMeans(clean_data) 
test <- cov(clean_data)
val <- mahalanobis(clean_data, x_val, test) 
sort_val <-sort(val)
quantiles <-qchisq((1:nrow(clean_data)-1/2)/nrow(clean_data), df=ncol(clean_data))
plot(quantiles, sort_val,
     xlab=expression(paste(chi[3]^2,"Quantile")),
     ylab="Ordered squared distances", main="") 
abline(a=0, b=1)
text(quantiles, sort_val, abbreviate(names(sort_val)), col="green", pch=0.8)


#Scatterplot of Clean Data
plot(clean_data)  
#scatterplot of clean data
```
Note: In the Multivariate normality test with clean data, even after removing the outlier countries, in the new plot, we can still see some outliers, and as much as I tried to remove each outlier, new countries popout with more deviation from the norm so I decided to remove only the above outliers for the sake of this analysis.

# 2.4 Dimension Reduction Analysis:Principle Component Analysis(PCA)
The "World Happiness" data consists of six variables that measure the happiness of 158 countries. Our goal is to find new variables, or principal components, that are uncorrelated with each other. We can apply principal component analysis (PCA) to determine if the first principal component (PC1) reflects the actual happiness of countries. After cleaning the data and ensuring that all variables are in the same direction, we can perform PCA. The output of this analysis shows that the first two components account for about 74% of the overall variance in the data. Generally, a cut-off value of 70% is used to choose PCAs. The loadings show the correlation between each original variable and the new principal components. In this case, the coefficients for PC1 are positive for all six original variables, indicating that PC1 represents the overall score across all six criteria of happiness. PC2 represents countries with high scores on GDP, social support, and health life expectancy but low scores on freedom, generosity, and perception of corruption. Singapore has the highest PC1 score (4.017209), although it has a happiness score of 6.262 and is ranked 34th in the original data. When Singapore is treated as an outlier, Norway has the highest PC1 score. The correlation between the first principal component score and the actual happiness score is 0.8812417, indicating that the first principal component is in agreement with the assigned scores for the countries. Therefore, the reality of the data can be represented with two new variables (principal components) instead of the six original variables.

```r
pca_happiness_data<- princomp(clean_data, cor=T) 
summary(pca_happiness_data, loading=T)
max(pca_happiness_data$scores)
pca_happiness_data$scores
#check correlation with the data
cor(clean_data$GDP.per.capita, pca_happiness_data$scores[,1])
#now we plot the PCA Biplot
biplot(pca_happiness_data, cex=0.5)
```
PCA-Biplot

A biplot is a scatterplot that aims to represent both the observations and variables of a matrix of multivariate data on the same plot, which helps to interpret the principal component axes while looking at the location of individual points. Projecting a data point onto the direction represented by an arrow gives the measurements of that variable for that data value. For example, New Zealand has high scores in all six criteria, while Kenya has higher scores on generosity, perception of corruption, and freedom, but lower scores on GDP per capita, health life expectancy, and social support. The loadings for PC2 indicate the correlation between the original variables and the new PC2 score, as shown by the angles. GDP per capita, health life expectancy, and social support have a positive angle, while freedom, generosity, and perception of corruption have a negative angle, as indicated by the PC2 loadings. The biplot shows that GDP per capita, social support, and health life expectancy are highly correlated with each other, as are generosity, perception of corruption, and freedom. Exploratory factor analysis (EFA) may demonstrate that these pairs of variables identify two latent factors. The length of the arrow indicates the impact of the variables on PC1 and PC2. From the plot, it appears that GDP per capita has the most impact on PC1 and PC2 scores.

# 2.5 Dimension Reduction Analysis:Multiple Dimension Scaling(MDS)
```r
s_dist <-dist(scale(clean_data)) 
mydata.mds <-cmdscale(s_dist, k=2, eig=T) 
cumsum(mydata.mds$eig)/sum(mydata.mds$eig)
# MDS for observations
plot(mydata.mds$points, pch='.', xlab ="Coordinate 1", ylab="Coordinate 2") 
text(mydata.mds$points, labels=rownames(clean_data), cex=0.5)
# MDS for variables
dist_corr<-1-cor(clean_data)
mydata.mds2 <-cmdscale((dist_corr), k=3, eig=T) 
plot(mydata.mds2$points,xlim=c(-1.2, 1.2), ylim=c(-1,1), pch='.', xlab ="Coordinate 1", ylab="Coordinate 2")
text(mydata.mds2$points, labels=colnames(clean_data), cex=0.5)
```

Looking at the plot for component 1 and component 2 for observations, countries with similar happiness score are in close proximity. For example, Denmark & Norway have similar characteristics and closer on map and they are very far from Malawi (country with low happiness score).

Multidimensional scaling (MDS) can also be used to represent variables when the distance matrix is derived from a correlation matrix. From the plot, GDP, social support, and health life expectancy are closer together, while perception of corruption, freedom, and generosity are closer together. This suggests that these two pairs of variables will likely be on different factors in an EFA.

# 2.6 Dimension Reduction Analysis:Canonical Correlation Analysis(CCA)
```r
X <- scale(clean_data[, 1:3]) 
Y<- scale(clean_data[, 4:6])
library(CCA) 
cca <- cc(X, Y) 
U<-cca$scores$xscores #U1 scores is the first column of xscores head(U)
V<-cca$scores$yscores #V1 scores is the first column of xscores head(V)
round(cca$cor, 3) 
tes <- cca$xcoef 
tes1<-tes[,1]/min(tes[,1])
tes1
sha<-cca$ycoef 
sha1<-sha[,1]/min(sha[,1])
sha1
```
Principal component analysis (PCA) considers the interrelationships within a set of variables, while canonical correlation analysis (CCA) assesses the relationships between two sets of variables. CCA works by maximizing the correlation between the pairs and the first pair always has the highest correlation. For example, if X represents social support, GDP, and life expectancy, and Y represents freedom, perception of corruption, and generosity, CCA would analyze the relationships between these two sets of variables.
Maximum possible pairs are 3, and correlation between first pair (U1, V1) is 0.509. With the X
and Y coefficients found through CCA, we can create a new surrogate variable. The linear
construct for the first pair is as shown:
$$U1 = x1 + 0.2486264x2 + 0.3869872x3$$
$$V1 = y1 - 0.6340328y2 + 0.9175176y3$$
As an interpretation, the analysis suggests that GDP has more dominance among the first set of variables (U1), while freedom has more dominance among the second set of variables (V2). This makes sense because a high GDP per capita may be correlated with a strong relationship to life expectancy and strong social support connections might contribute to overall happiness. Additionally, a country with more individual freedom might have higher trust in government.
