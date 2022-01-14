### Title:    Regression in R: Lab 1 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2022-01-14


###-Overview-----------------------------------------------------------------###

## You will practice basic regression modeling and model comparison.

## You will need the "longley.rds" dataset. This dataset is available in the
## "data" directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) Use the "install.packages" function to install the "MLmetrics" package.

## 2) Use the "library" function to load the "MLmetrics" packages.

## 3) Use the "paste0" function and the "readRDS" function to load the
##    "longley.rds" data into your workspace.


###-Linear Regression--------------------------------------------------------###

### Use the "longley" data for the following:

## 1a) Regress "GNP" onto "Year".
## 1b) What is the slope of "Year" on "GNP"?
## 1c) Is the effect of "Year" on "GNP" statistically significant at the
##     alpha = 0.05 level?
## 1d) Is the effect of "Year" on "GNP" statistically significant at the
##     alpha = 0.01 level?

## 2a) Regress "GNP" onto "Year" and "Population".
## 2b) Is there a significant effect (at the alpha = 0.05 level) of "Year" on
##     "GNP", after controlling for "Population"? 
## 2c) Is there a significant effect (at the alpha = 0.01 level) of "Year" on
##     "GNP", after controlling for "Population"? 
## 2d) What is the 95% confidence interval (CI) for the partial effect of
##     "Population" on "GNP"?

## 3a) Regress "GNP" onto "Year" and "Employed".
## 3b) What is the partial effect of "Employed" on "GNP", after controlling for
##     "Year"? 
## 3c) Is the partial effect of "Employed" on "GNP" statistically significant at
##     the alpha = 0.05 level?
## 3d) What is the 99% CI for the partial effect of "Employed" on "GNP"?

## 4a) Regress "GNP" onto "Year" and "Unemployed".
## 4b) What is the partial effect of "Unemployed" on "GNP", after controlling
##     for "Year"? 
## 4c) Is the partial effect of "Unemployed" on "GNP" statistically significant
##     at the alpha = 0.05 level?


###-Model Comparison---------------------------------------------------------###

### Use the "longley" data for the following:

## 1a) What is the difference in R-squared between the simple linear regression
##     of "GNP" onto "Year" and the multiple linear regression of "GNP" onto
##     "Year" and "Population"?
## 1b) Is this increase in R-squared significantly different from zero at the
##     alpha = 0.05 level?
## 1c) What is the value of the test statistic that you used to answer (1b)?

## 2a) What is the MSE for the model that regresses "GNP" onto "Year" and
##     "Employed"?
## 2b) What is the MSE for the model that regresses "GNP" onto "Year" and
##     "Unemployed"?
## 2c) According to the MSE values calculated above, is "Employed" or
##     "Unemployed" the better predictor of "GNP"?
## 2d) Could we do the comparison in (2c) using an F-test for the difference in
##     R-squared values? Why or why not?
