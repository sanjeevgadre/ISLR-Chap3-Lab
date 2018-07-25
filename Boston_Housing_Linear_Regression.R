## Loading necessary libraries
library(MASS)
library(ISLR)
library(tidyverse)

##Simple Linear Regression with medv as the response and lstat as predictor
attach(Boston)
lm_fit_1 = lm(medv~lstat)
CIs = confint(lm_fit_1)

##Predictions based on lm_fit_1, first confidence intervals and then prediction intervals
Prediction_1_1 = predict(lm_fit_1, data.frame(lstat = c(5, 10, 15)),interval="confidence")
Prediction_2_1 = predict(lm_fit_1, data.frame(lstat = c(5, 10,15)), interval="prediction")

##Plotting lm_fit_1
plot_lm_fit_1 = Boston %>% ggplot(aes(lstat, medv))+ geom_point()+ geom_abline(intercept=lm_fit_1$coefficients[1], slope=lm_fit_1$coefficients[2], col="red", lwd=1)

##Identifying the quality of fit

##Storing fit details in a dataframe
lm_fit_1_df = data.frame(fitted_values=predict(lm_fit_1), residuals=residuals(lm_fit_1), hatvalues=hatvalues(lm_fit_1))

## 1. Non-linearity in response-predictor function. Plot residuals with fitted values
lm_fit_1_quality_1 = lm_fit_1_df %>% ggplot(aes(fitted_values, residuals))+ geom_point(color="blue")+ geom_smooth(method="loess", level=0, color="red")

## 2. Identifying high leverage points. Plot hatvalues (leverage statistic) with fitted values
lm_fit_1_quality_2 = lm_fit_1_df %>% ggplot(aes(hatvalues, residuals))+ geom_point(color="blue"); lm_fit_1_quality_2
                            