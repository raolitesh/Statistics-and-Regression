# Building a Model
We are trying to build a model to identify the factors linearly influencing the happiness score of a country. Dependent variable is happiness score and independent
variables are GDP per capita, social support, life expectancy, freedom, generosity, corruption.
```r
### Multiple linear regression model with all parameters included.
names(happiness)
lm_happy <- lm(
  formula = Score ~ GDP_Capita + Social_Support +
    Life_Expectancy + Freedom +
    Generosity + Corruption,
  data = happiness
)
summary(lm_happy)
```
This model summary shows that only generosity is not a strong predictor of happiness at a significance level of 0.1, while perceptions of corruption is not significant at the 0.05 level. The R-squared and adjusted R-squared values are both close to 1, indicating that the model is a good fit for the data. The values of these two measures are also similar, indicating that the model is not overfitting to the data.
# ANOVA Test

Let's run an ANOVA test to test the usefulness of our predictors.
```r
options(scipen=-100, digits = 3)
anova(lm_happy)
```
The ANOVA test shows that generosity is not a significant predictor of happiness at a p-value of 0.13 and perceptions of corruption is not significant at a p-value of 0.075.
# Testing Multicollinearity
**Original Model VIF**
```r
options(scipen=10, digits=3)
vif <- round(car::vif(lm_happy),2)

cat("VIF of Original model\n")
cat("##########################\n\n")

cat("GDP Per Capita: ", vif[1])
cat("\nSocial Support: ", vif[2])
cat("\nLife Expectancy: ", vif[3])
cat("\nFreedom: ", vif[4])
cat("\nGenerosity: ", vif[5])
cat("\nPerceptions of Corruption: ", vif[6])

paste0("R-squared: ", round(summary(lm_happy)$r.squared, 2))
```
**Updated Model VIF**
```r
lm_new <- update(
    object = lm_happy,
    formula = Score ~ .  - Generosity - Healthy.life.expectancy - Perceptions.of.corruption)
summary(lm_new)
anova(lm_new, lm_happy)
```
```r
cat("VIF of New model\n")
cat("#################\n\n")
vif_2 <- round(car::vif(lm_new), 2)

cat("GDP Per Capita: ", vif_2[1])
cat("\nSocial Support: ", vif_2[2])
# cat("\nLife Expectancy: ", vif_2[3])
cat("\nFreedom: ", vif_2[3])
# cat("\nPercpetions of Corruption: ", vif_2[4])
```
We found that GDP had high levels of multicollinearity with other variables, so we removed the healthy life expectancy variable to address this issue. However, we kept the social support variable because removing it significantly decreased the R-squared value. Additionally, we removed generosity and perceptions of corruption because they did not significantly impact a country's happiness score according to their p-values and ANOVA report.
# Distribution of Residuals
Another way to test the reliability of our model is to analyze the distribution of it's residuals. We will do this by looking at Q-Q plots, histograms, and by running skewness and Shapiro-Wilk tests.
**Q-Q Plot**
```r
par(mfrow=c(2,2))
qqnorm(lm_happy$residuals);qqline(lm_happy$residuals)
qqnorm(lm_new$residuals);qqline(lm_new$residuals)
```
Both plots show the residuals approximately lie on the Q-Q line with some variance.
**Histograms**
```r
options(repr.plot.width = 14, repr.plot.height = 12)
par(mfrow=c(2,2))
h <- hist(
        x = lm_happy$residuals,
        xlab = "Residuals of Original Model",
        ylab = "Count",
        main = "Histogram of Original Model's Residuals",
        las = 1,
        ylim = c(0,65),
        col = heat.colors(8)
)
text(
    x = h$mids,
    y = h$counts,
    labels = h$counts,
    adj = c(0.5,-0.5)
)

h1 <- hist(
        x = lm_new$residuals,
        xlab = "Residuals of New Model",
        ylab = "Count",
        main = "Histogram of New Model's Residuals",
        las = 1,
        xlim = c(-3, 2),
        col = heat.colors(8)
)
text(
    x = h1$mids,
    y = h1$counts,
    labels = h1$counts,
    adj = c(0.5,-0.5)
)
```
**Skewness**
```r
cat("##### Skewness of models #####\n\n")

cat("Skewness of original model: ", round(skewness(lm_happy$residuals),3))
cat("\nSkewness of new model: ", round(skewness(lm_new$residuals),3))
```
The updated model is slightly less skewed than the original.
```r
shapiro.test(lm_happy$residuals)
shapiro.test(lm_new$residuals)
```
Both models' residuals are not normally distributed as neither have a p-value > 0.5.
# Conclusion for this part of the analysis

In summary, our analysis has shown that GDP per capita, social support, and healthy life expectancy are the main factors that contribute to a country's overall happiness. Increasing these factors can lead to higher happiness scores for a country. Additionally, we found that freedom and perceptions of corruption also have an impact on happiness, although to a lesser extent. Finally, we found that generosity did not have a significant effect on happiness according to our analysis.

Overall, the analysis suggests that increasing GDP and promoting freedom are important factors in improving a country's happiness score. Improving financial situations and increasing autonomy may lead to improvements in other areas such as social support and life expectancy, which also contribute to happiness. It may be beneficial to focus resources and efforts on these areas in order to increase overall happiness in countries with lower scores.
