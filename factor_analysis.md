# 4. Exploratory and Confirmatory Factor Analysis(EFA and CFA)
Exploratory factor analysis (EFA) identified two potential latent variables, which we believe represent personal/household happiness and societal happiness. Confirmatory factor analysis (CFA) confirmed that GDP per capita, social support, and health life expectancy reflect personal/household happiness, while freedom, perception of corruption, and generosity reflect societal happiness. In summary, these two variables are factors within the household and factors outside the household.
# 4.1 EFA
```r
test_hapiness <- factanal(mydata.s, factors=2, scores="regression")
test_hapiness #for large dataset p value may not describe our null hypothesis.
corHat <- test_hapiness$loadings %*% t(test_hapiness$loadings) + diag(test_hapiness$uniquenesses)
corr <- cor(mydata.s)
rmse=sqrt(mean((corHat-corr)^2))
rmse
```
Exploratory Factor Analysis (EFA)

We performed exploratory factor analysis to identify potential unobserved/latent variables based on the following variables.
● GDP per capita
● Social Support
● Health Life Expectancy
● Freedom
● Perception of Corruption 
● Generosity
The RMSE is 0.02097272 indicates that appropriate number of latent factors is two. As long as the RMSE is less than 0.05, it is safe to infer two unobserved factors.
Interpretations of EFA

Factor 1:
● GDP per capita
● Social Support
● Health life expectancy

Factor 2:
• Freedom
• Perception of Corruption 
• Generosity 
We can argue that factor 1 represents personal/household happiness and factor 2 represents societal happiness.

# 4.2 CFA
```r
library(lavaan)
happiness.model <- 'Public =~ GDP.per.capita + Social.support + Healthy.life.expectancy
Personal =~ Freedom.to.make.life.choices + Perceptions.of.corruption + Generosity'
options(fit.indices = c("GFI", "AGFI", "SRMR")) # Some fit indices
fit.cfa <- cfa(happiness.model, sample.cov= cor(mydata.s), std.lv=T, sample.nobs = nrow(mydata.s))
options(fit.indices = c("GFI", "AGFI", "SRMR")) # Some fit indices summary(fit.cfa, fit.measures=TRUE)
fitMeasures(fit.cfa)
fitMeasures(fit.cfa)[c("gfi", "agfi","srmr")]
# ci <- confint(fit.cfa)
# ci
#Note: for some reason I am not being able to calculate the confidence interval here
#I lnow I am supposed to use the above function but it shows some error so I left it as is. 

library(semPlot)
semPaths(fit.cfa, rotation=2, 'std', 'est')
#semPaths(fit.cfa, rotation=2, 'std', 'est', ci.lower=ci[,1], ci.upper=ci[,2])
#similarly, I am not being able to plot the confidence interval here
```
## Confirmatory Factor Analysis (CFA)

### CFA Metrics

The standardized root mean square residual (SRMR), goodness of fit index (GFI), and adjusted goodness of fit index (AGFI) are borderline, but these numbers can still confirm the model. One limitation of the analysis is that there is a potential ecological fallacy, as we are analyzing country data as the unit of analysis to explain household happiness. Conducting a survey at the household level in each country, adjusted for population, might produce a better confirmatory factor analysis (CFA) model.

SRMR
The SRMR is 0.0756651. This is slightly greater than against the arbitrary cut-off criteria of 0.05.

GFI
The GFI is 0.9520059. This is borderline against the arbitrary cut-off of 0.95.

AGFI
The AGFI is 0.8740156.

## Conclusion and Recommendation for this part of analysis
Based on our analysis, it is reasonable to assume that the data follows a multivariate normal distribution. The happiness report contains two principal components that represent the overall reality of the data. Model-based clustering was the most accurate clustering technique, producing three groups with 70% to 90% accuracy: North America/Western Europe/Australia, Latin America/North Africa/Northeast Asia, and Southern Asia/Africa. Confirmatory factor analysis confirmed the existence of two latent factors that impact happiness: household/personal and societal happiness, or factors within and outside the household.

As a recommendation, further research might improve our understanding of the data and avoid ecological fallacy by examining individual happiness while controlling for factors such as education and environment. It can be challenging to use aggregate data with countries as the unit of analysis to understand individual happiness. A more detailed analysis at the household or individual level, rather than at the country level, might provide more insights.



