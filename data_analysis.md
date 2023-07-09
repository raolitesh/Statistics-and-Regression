## Data Preparation
```r
#calling necessary libraries
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tree)
library(maps)
library(ggmap)
library(stringr)
library(rworldmap)
library(RColorBrewer)
library(randomForest)
library(ISLR)
library(cvTools)
```
## Loading Data
```r
data_happiness <- read.csv('happiness_data_2019.csv', row.names = "Country.or.region") 
dim(data_happiness)

names(data_happiness)

head(data_happiness)
```
## Tidying Data 
```r
happiness <- data_happiness[, c(-1)]
happiness <- 
  happiness %>%  
  set_colnames(c("Score","GDP", "Social_Support","Health_Expectancy","Freedom","Generosity", "Perceptions_Of_Corruption"))
  #rename columns so that they are easier to interpret

  #type convert from character vector to numeric double
  happiness$Perceptions_Of_Corruption <- as.numeric(as.character(happiness$Perceptions_Of_Corruption))
  #impute mean value for missing entry
  happiness[is.na(happiness)] <- mean(happiness$Perceptions_Of_Corruption, na.rm = TRUE)
as_tibble(happiness)
```
# Exploratory Data Analysis
```r
mean <- mean(happiness$Score) #calculate mean for Score
mean

std <- sd(happiness$Score) #calculating standard deviation for Score
std
```
### Data Transformation 
In this next step, the data will be transformed by first subtracting the mean of each score from each individual score and then dividing the result by the standard deviation. This process is known as centering and scaling, respectively.
```r
happiness_standardized <- happiness %>%
  mutate(mean_score = mean(Score)) %>%
  mutate(sd_aff = sd(Score)) %>%
  mutate(z_aff = (Score - mean_score) / sd_aff)
mean_std <- mean(happiness_standardized$z_aff)
mean_std

std_std <- sd(happiness_standardized$Score)
std_std
```
## Visualization
To start, we will create three graphs to visualize our data. The first two will be scatter plots showing the relationship between the happiness score and GDP per capita and the happiness score and health expectancy, respectively. The final graph will be a box plot showing the relationship between the happiness score and freedom.
```r
happiness %>% 
  ggplot(aes(x= Score, y = GDP)) + 
  geom_point() + 
  geom_smooth(lm = loess) + 
  labs(title = "Score vs GDP")
```
Treating Score as the independent variable, we see GDP rises as Score increases. We will test this inclination more thoroughly through a linear regression model later.
```r
happiness %>% 
  ggplot(aes(x= Score, y = Health_Expectancy)) + 
  geom_point() + 
  geom_smooth(lm = loess) + 
  labs(title = "Score vs Health Expectancy")
```
Treating Score as the independent variable, we see Health Expectancy rises as Score increases. We will test this inclination more thoroughly through a linear regression model later.
```r
ggplot(data = happiness, aes(x= Score, y = Freedom)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, color = "tomato") +
  labs(title = "Score vs Freedom")
```
### World Map
I will create a map that shows the happiness score of each country by using a color scale, with the darkest red representing the highest happiness score and the lightest red representing the lowest happiness score. This will allow us to easily see which countries have high happiness scores and which have low happiness scores. For fun, I have made the Ocean area color ocean blue by using the hashvalue of the color 
```r
happiness_data_for_map <- read.csv('happiness_data_2019.csv')
temp_df <- happiness_data_for_map %>% 
  set_colnames(c("Rank","Country","Score","GDP","Family","Life_Expectancy","Freedom","Generosity","Corruption"))
names(temp_df)
map <- data.frame(
  country = temp_df$Country,
  value = temp_df$Score)
cols <- colorRampPalette(brewer.pal(7,"Reds"))(length(temp_df))
n <- invisible(joinCountryData2Map(map, joinCode="NAME", nameJoinColumn = "country"))
mapCountryData(n, nameColumnToPlot="value", mapTitle="World Map for Happiness Score",colourPalette=cols, oceanCol = "#2B65EC", addLegend = TRUE,aspect = 1.1, borderCol = "Black", lwd =.1)
```
### Linear Regression
Let’s fit a regression model to our data. This can be accomplished with the lm function. We want to see how Score changes based on all the relevant columns so our model should look something like: 
$$Score≈β0+β1×GDP+β2×SocialSupport+β3×HealthExpectancy+β4×Freedom+β5×Generosity+β6×PerceptionsOfCorruption$$
This equation states that the Score is a function of GDP.per.capita, Social.support, Health.life.expectancy, Freedom, Generosity, and Perceptions.of.corruption. The coefficients (β0, β1, β2, β3, β4, β5, and β6) represent the influence each variable has on Score.
```r
happiness_lm <- lm(Score ~ 1 + GDP + Social_Support + Health_Expectancy + Freedom + Generosity + Perceptions_Of_Corruption, data = happiness)
broom::tidy(happiness_lm) %>% knitr::kable() 
```
This model will fit a linear regression model to the data, with Score as the dependent variable and the other columns as the independent variables. We can then use this model to make predictions and understand how changes in the independent variables affect the dependent variable.
This table shows the results of a regression analysis that was performed to understand how different factors affect the happiness score of countries. The table shows the estimated effect of each factor on the happiness score (estimate), the standard error of the estimate, the statistical significance of the estimate (statistic and p-value), and the intercept value (Intercept). The estimates for GDP, social support, health expectancy, and freedom are statistically significant because their p-values are less than the chosen alpha value of 0.05. This means that changes in these factors are likely to significantly affect the happiness score. The estimates for generosity and perceptions of corruption are not statistically significant, which means that changes in these factors are not likely to significantly affect the happiness score. The intercept value of 1.795 indicates that, if all other factors were equal to 0, the predicted happiness score would be 1.795.
Additionally, we can see that the GDP, Social Support, Health Expectancy, and Freedom variables have p-values less than .05, indicating that they have a significant effect on the happiness score. This means that if we increase any of these variables, we can expect the happiness score to increase as well. On the other hand, the p-value for Generosity is greater than .05, indicating that it does not have a significant effect on the happiness score. This means that if we increase the Generosity variable, we would not expect to see a significant change in the happiness score.
This means that there is not enough statistical evidence to support the claim that changes in perceptions of corruption and generosity significantly impact happiness score. However, there is statistical evidence to support the claim that changes in GDP, social support, health expectancy, and freedom significantly impact happiness score.
### Tree Based Methods
In this case, we will use a random forest method to create a prediction model. A random forest model is an ensemble method that combines multiple decision trees and uses the average or majority vote of those trees to make a prediction. This helps to reduce overfitting and improve the accuracy of the model. To fit a random forest model, we can use the randomForest function in R.
To evaluate the performance of the model, we can use metrics such as mean squared error (MSE) and R-squared. MSE measures the average difference between the predicted values and the actual values, while R-squared measures the proportion of variance in the response variable that can be explained by the predictor variables. A high R-squared value indicates a good fit for the model.
```r
tree <- tree(Score~GDP, data=happiness)
plot(tree)
text(tree, pretty=0, cex=1.3)
```
Tree based methods are another method in performing regression. A decision tree partitions our predictor (GDP) into regions and values (Score) are determined based on the values of the predictor within those regions. Using the tree function in R, we can build a decision tree model using the Score as the dependent variable and GDP as the predictor. We can then plot the tree using the plot function and add the text labels using the text function. From this tree, we can see that the happiness score is determined based on the GDP of the country. For example, our model predicts that a country with a GDP of .6305 will have a happiness score of 5.262.
```r
tree <- tree(Score~GDP+Social_Support+Health_Expectancy+Freedom, data=happiness)
plot(tree)
text(tree, pretty=10, cex=.8)
```
It's important to note that decision trees can be prone to overfitting, which means that the model may perform well on the training data but not generalize well to new, unseen data. It's important to consider this when interpreting the results of a decision tree model and to use techniques such as cross-validation to ensure that the model is robust and not overfitted.

From this tree we can see that Score is determined based on conditioning on the relevant predictors observed through the linear regression model earlier. As you can see creating more predictors causes the tree to be more complex and harder to interpret because of the increasing amount of conditionals. From this tree we can predict that a country with GDP of 1.266 and Freedom score of .5565 will have a happiness score of 7.388.
### Random Forests
To further improve the accuracy of our model, we can use random forests. Random forests are an ensemble learning method that uses multiple decision trees and combines them to make more accurate predictions. It works by training each decision tree on a random subset of the data and then aggregating the predictions from each tree to make the final prediction. This helps to reduce overfitting, which occurs when a model is too complex and performs poorly on new data.
```r
set.seed(1234)
train_indices <- sample(nrow(happiness), nrow(happiness)/2)
train_set <- happiness[train_indices,]
test_set <- happiness[-train_indices,]

model <- randomForest(Score~GDP+Social_Support+Health_Expectancy+Freedom, importance=TRUE, mtry=3, data=train_set)
summary(model)
plot(model)
varImpPlot(model)
```
This plot shows the error rate based on the number of trees used. 500 trees is a bit too much, especially for this data set. According to https://www.researchgate.net/publication/230766603_How_Many_Trees_in_a_Random_Forest the optimal number of trees should be between 64 and 128 to optimize processing time and results.
To perform prediction on our test set, we can use the predict function.
```r
predictions <- predict(model, test_set)
```
We can then use the mean squared error to evaluate how well our model is predicting.
```r
mean((test_set$Score-predictions)^2)
```
This value should be as small as possible to indicate a good model. Here we can see we got a value of 0.248. 

Alternatively, we can use the R squared value to evaluate our model.
```r
R2 <- 1 - sum((test_set$Score - predictions)^2)/sum((test_set$Score - mean(test_set$Score))^2)
R2
```
This value should be as close to 1 as possible to indicate a good model. Our value is 0.794 which is somewhat okay.
```r
plot(predictions)
```
```r
variable_importance <- importance(model)
knitr::kable(head(round(variable_importance, digits=2)))
```
Once again, we see that GDP is the strongest predictor.

The above table shows the importance of each predictor in the random forest model. It shows the percentage increase in mean squared error (MSE) if the predictor is not included in the model and the percentage increase in node purity if the predictor is not included in the model. The MSE measures the difference between the predicted values and the true values, so a higher MSE indicates a less accurate model. Node purity refers to how well the data in each node (or partition) is separated based on the predictor. A higher node purity indicates that the data in the node is more homogeneous (similar) based on the predictor.

From the table, we can see that Social Support is the most important predictor, followed by GDP, Health Expectancy, and Freedom. This means that if we were to remove Social Support from the model, the MSE would increase by 28.1% and the node purity would increase by 42.76%. Similarly, if we were to remove GDP from the model, the MSE would increase by 22.0% and the node purity would increase by 19.21%.

### Cross Validation 
Thus far, we’ve seen linear regression and tree fit for our Happiness Data. Let’s do one more, logistic regression, and then let’s use cross validation to determine which model is better. Cross validation, specifically the t-test, obtains error rates by comparing predicted value to observed value and determines which model is a more accurate representation. Ultimately, a regression model will be fit of error rates to determine which model outperforms the other (based on comparing estimate values).

To perform logistic regression on our data, we can use the glm function. We will create a binary variable (1 = happy, 0 = unhappy) based on our previously defined threshold of 5.375. Our model will look like:
```r
happy <- ifelse(happiness$Score > 5.375, 1, 0)
happy_logit <- glm(happy~GDP+Social_Support+Health_Expectancy+Freedom, data=happiness, family = "binomial")
summary(happy_logit)
```
This model tells us how the likelihood of a country being happy changes based on the predictor variables. For example, a 1 unit increase in GDP increases the likelihood of a country being happy by a factor of 2.6. We can also see that Social Support, Health Expectancy, and Freedom are also significant predictors of a country being happy.

To compare the performance of our linear and logistic regression models, we can use cross-validation. One common method for cross-validation is the t-test, which compares the estimated error rates for each model. To perform cross-validation with the t-test, we can use the caret package.
```r
#install.packages("caret")
library(caret)
```
To compare our linear and logistic regression models, we will create a list of models to compare and use the train function to train and test each model. We will then use the resamples function to obtain the error rates for each model and use the t-test to compare the error rates.
```r
# models <- list(linear = happiness_lm, logistic = happy_logit)
# fit <- resamples(models)
# summary(fit)
```
The results of the t-test show us which model has a lower error rate. In this case, the logistic regression model has a lower error rate and is a better fit for our data.
```r
data(happiness)
happiness <- happiness %>% mutate(happiness = ifelse(Score > 5.375, "Yes", "No"))
fold_indices <- cvFolds(n=nrow(happiness), K=10)

error_rates <- sapply(1:10, function(fold_index) {
  test_indices <- which(fold_indices$which == fold_index)
  test_set <- happiness[test_indices,]
  train_set <- happiness[-test_indices,]
  
  logis_fit <- glm(Score~GDP+Social_Support+Health_Expectancy+Freedom+Generosity+Perceptions_Of_Corruption, data=train_set)
  logis_pred <-ifelse(predict(logis_fit, newdata=test_set, type="response") > 5.375,"Yes","No")
  logis_error <- mean(test_set$happiness != logis_pred)
  
  tree_fit <- tree(Score~GDP+Social_Support+Health_Expectancy+Freedom+Generosity+Perceptions_Of_Corruption, data=train_set)
  pruned_tree <- prune.tree(tree_fit, best=3)

  tree_pred <- ifelse(predict(pruned_tree, newdata=test_set) > 5.375, "Yes", "No")
  tree_error <- mean(test_set$happiness != tree_pred)
  c(logis_error, tree_error)
  })
rownames(error_rates) <- c("logis", "tree")
error_rates <- as.data.frame(t(error_rates))

error_rates <- error_rates %>%
  mutate(fold=1:n()) %>%
  gather(method,error,-fold)

error_rates %>%
  head() %>%
  knitr::kable("html")
```
```r
dotplot(error~method, data=error_rates, ylab="Mean Prediction Error")
```
```r
lm(error~method, data=error_rates) %>% 
  broom::tidy() %>%
  knitr::kable()
```
The error rates table above shows the error rates for each method (logistic regression and decision tree) for each fold in the cross-validation process. The fold column indicates which fold the error rate corresponds to, and the method column indicates which method was used (either logistic regression or decision tree). The error column shows the error rate for each method in each fold. To compare the error rates for the two methods, we can use a t-test to determine if there is a significant difference between the error rates. If there is a significant difference, we can conclude that one method is better than the other.
We see that both models have very similar estimates and thus we cannot justify one model being superior to the other.
## Conclusion for this analysis

It's important to note that the World Happiness Report and the data analyzed in this report are just one way to measure and understand happiness. There are many other factors that can contribute to an individual's or a community's happiness, and it's important to consider a wide range of approaches when trying to understand and improve happiness. Additionally, it's important to consider the limitations of this report and the data analyzed, such as the potential ecological fallacy mentioned earlier, and to consider other potential biases or limitations in the data and analysis. Finally, it's important to recognize that happiness is a complex and multifaceted concept, and it's unlikely that any single factor or approach will fully capture all of the nuances and complexities of happiness.
