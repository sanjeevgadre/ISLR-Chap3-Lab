## Loading necessary libraries and data
library(MASS)
library(ISLR)
library(tidyverse)
library(car)
attach(Boston)

##Simple Linear Regression with medv as the response and lstat as predictor
lm_fit_1 = lm(medv~lstat, data=Boston)
CIs = confint(lm_fit_1) ##Confidence Intervals

##Predictions based on lm_fit_1, first confidence intervals and then prediction intervals
Prediction_1_1 = predict(lm_fit_1, data.frame(lstat = c(5, 10, 15)),interval="confidence")
Prediction_2_1 = predict(lm_fit_1, data.frame(lstat = c(5, 10,15)), interval="prediction")

##Plotting lm_fit_1
plot_lm_fit_1 = Boston %>% ggplot(aes(lstat, medv))+ geom_point()+ geom_abline(intercept=lm_fit_1$coefficients[1], slope=lm_fit_1$coefficients[2], col="red", lwd=1)

##Identifying the quality of fit

lm_fit_1_df = data.frame(fitted_values=predict(lm_fit_1), residuals=residuals(lm_fit_1), hatvalues=hatvalues(lm_fit_1)) ##Storing fit details in a dataframe

    ## 1. Non-linearity in response-predictor function. Residuals v/s fitted values
lm_fit_1_quality_1 = lm_fit_1_df %>% ggplot(aes(fitted_values, residuals))+ geom_point(color="blue")+ geom_smooth(method="loess", level=0, color="red")

    ## 2. Identifying high leverage points. 
    ##    Ratio of hatvalues (leverage statistic) to average leverage of all values
lm_fit_1_df = lm_fit_1_df %>% mutate(hatv_to_avg_hatv=hatvalues/(1+1/length(hatvalues)))
lm_fit_1_quality_2 = lm_fit_1_df %>% ggplot(aes(hatv_to_avg_hatv))+ geom_histogram(fill="blue");

##Modelling multiple linear regressions

    ##Using 2 variables as predictors
lm_fit_5 = lm(medv~lstat+age, data=Boston) 

    ##Using all variables as predictors
lm_fit_6 = lm(medv~., data=Boston) 

    ##Excluding 2 variables, indus and age, that were found to have insignificant impact on the response from the earlier regression.
lm_fit_7 = lm(medv~.-indus-age, data=Boston) 

##Modelling non-linear transformations of the predictors
lm_fit_8 = lm(medv~lstat+I(lstat^2), data=Boston)

    ##Including multiple powers of predictor
lm_fit_9 = lm(medv~poly(lstat,5), data=Boston) 

    ##Including interactions between predictors
lm_fit_10 = lm(medv~.-indus-age+dis:tax, data=Boston)

#Modellling to include qualitative predictors
lm_fit_11 = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)

    ##Excluding predictors, Population, Education, Urban, US and Price:Age
lm_fit_12 = update(lm_fit_11, ~.-Population-Education-Urban-US-Price:Age)

#Identifying the quality of multiple regression fit
    ##1. Collinearity between predictors through variance inflation factor (VIF)
lm_fit_7_quality_1 = vif(lm_fit_7)

#Identifies rad and tax as two variables with high VIF values and therefore collinear with some other predictors. Dropping the two variables one at a time reveals that dropping tax provides better results.

##Updating the fit by eliminating variables, rad and tax, with high VIF values
lm_fit_7_new = update(lm_fit_7, ~.-tax)
