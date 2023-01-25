### Title:    Regression in R 1: Basics
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2023-01-25


###-Preliminaries------------------------------------------------------------###

rm(list = ls(all = TRUE)) # Clear workspace

library(MLmetrics) # For MSEs
library(dplyr)     # For pipelines


###-Data I/O-----------------------------------------------------------------###

dataDir <- "data/"
cars    <- readRDS(paste0(dataDir, "mtcars.rds"))

################################################################################
## PRACTICE PROBLEM 1.1
##
## Use the readRDS() function to load the "longley.rds" data.
##
## NOTE: Unless otherwise specified, these data are used for all subsequent
##       practice problems.
##
################################################################################


###-Simple Linear Regression-------------------------------------------------###

## Fit a simple linear regression model:
out1 <- lm(qsec ~ hp, data = cars)

## See what's inside the fitted lm object:
ls(out1)

## Summarize the results:
summary(out1)

## The 'summary' output is an object that we can save to a variable:
s1 <- summary(out1)
ls(s1)
s1

## Access the R^2 slot in the summary object:
s1$r.squared

## Extract coefficients:
coef(out1)

## Extract residuals:
res3 <- resid(out1)
res3

## Extract fitted values (two ways):
yHat1.1 <- fitted(out1)
yHat1.2 <- predict(out1)

yHat1.1
yHat1.2

## Compare:
yHat1.1 - yHat1.2

################################################################################
## PRACTICE PROBLEM 1.2
##
## a) Regress "GNP" onto "Year".
## b) What is the slope of "Year" on "GNP"?
## c) Is the effect of "Year" on "GNP" statistically significant at the
##    alpha = 0.05 level?
## d) Is the effect of "Year" on "GNP" statistically significant at the
##    alpha = 0.01 level?
##
################################################################################


###-Multiple Linear Regression-----------------------------------------------###

## Fit a multiple linear regression model:
out4 <- lm(qsec ~ hp + wt + carb, data = cars)

## Summarize the model:
s4 <- summary(out4)
s4

## Extract R^2:
s4$r.squared

## Extract F-stat:
s4$fstatistic

## Extract coefficients:
coef(out4)

## Compute confidence intervals for coefficients:
confint(out4)
confint(out4, parm = "hp")
confint(out4, parm = c("(Intercept)", "wt"), level = 0.99)

## Manually compute t-statistics:
cf4 <- coef(out4)
se4 <- out4 %>% vcov() %>% diag() %>% sqrt()
t4  <- cf4 / se4

s4
t4

################################################################################
## PRACTICE PROBLEM 1.3
##
## a) Regress "GNP" onto "Year" and "Population".
## b) Is there a significant effect (at the alpha = 0.05 level) of "Year" on
##    "GNP", after controlling for "Population"?
## c) Is there a significant effect (at the alpha = 0.01 level) of "Year" on
##    "GNP", after controlling for "Population"?
## d) What is the 95% confidence interval (CI) for the partial effect of
##    "Population" on "GNP"?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 1.4
##
## a) Regress "GNP" onto "Year" and "Employed".
## b) What is the partial effect of "Employed" on "GNP", after controlling for
##    "Year"?
## c) Is the partial effect of "Employed" on "GNP" statistically significant at
##    the alpha = 0.05 level?
## d) What is the 99% CI for the partial effect of "Employed" on "GNP"?
##
################################################################################


###-Model Comparison---------------------------------------------------------###

## Change in R^2:
s4$r.squared - s1$r.squared

## Significant increase in R^2?
anova(out1, out4)

## Compare MSE values:
mse1 <- MSE(y_pred = predict(out1), y_true = cars$qsec)
mse4 <- MSE(y_pred = predict(out4), y_true = cars$qsec)

mse1
mse4

################################################################################
## PRACTICE PROBLEM 1.5
##
## a) What is the difference in R-squared between the simple linear regression
##    of "GNP" onto "Year" and the multiple linear regression of "GNP" onto
##    "Year" and "Population"?
## b) Is this increase in R-squared significantly different from zero at the
##    alpha = 0.05 level?
## c) What is the value of the test statistic that you used to answer (b)?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 1.6
##
## a) What is the MSE for the model that regresses "GNP" onto "Year" and
##    "Population"?
## b) What is the MSE for the model that regresses "GNP" onto "Year" and
##    "Employed"?
## c) According to the MSE values calculated above, is "Population" or
##    "Employed" the better predictor of "GNP"?
## d) Could we do the comparison in (c) using an F-test for the difference in
##    R-squared values? Why or why not?
##
################################################################################


###-A More Elegant Way to Build Models---------------------------------------###

## The long way:
out1 <- lm(qsec ~ hp, data = mtcars)
out2 <- lm(qsec ~ hp + wt, data = mtcars)
out3 <- lm(qsec ~ hp + wt + gear + carb, data = mtcars)

## The short way:
out2.1 <- update(out1, ". ~ . + wt")
out3.1 <- update(out2.1, ". ~ . + gear + carb")
out3.2 <- update(out1, ". ~ . + wt + gear + carb")

## We can also remove variables:
out1.1 <- update(out2, ". ~ . - wt")

summary(out2)
summary(out2.1)

all.equal(out1, out1.1)
all.equal(out2, out2.1)
all.equal(out3, out3.1)
all.equal(out3, out3.2)

## We can rerun the same model on new data:
mtcars2 <- mtcars[1 : 15, ]

out4 <- update(out3, data = mtcars2)

summary(out3)
summary(out4)


###-END----------------------------------------------------------------------###
