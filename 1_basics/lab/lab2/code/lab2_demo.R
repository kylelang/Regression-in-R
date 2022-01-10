### Title:    Stats & Methods Lab 2 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2020-09-09

###--------------------------------------------------------------------------###

### Preliminaries ###

install.packages("MLmetrics", repos = "http://cloud.r-project.org")

rm(list = ls(all = TRUE)) # Clear workspace

library(MLmetrics) # We'll need this for MSEs

setwd("") # Let's all set our working directory to the correct place

###--------------------------------------------------------------------------###

### Data I/O ###

dataDir <- "../data/"

cars <- readRDS(paste0(dataDir, "mtcars.rds"))

###--------------------------------------------------------------------------###

### Simple Linear Regression ###

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

###--------------------------------------------------------------------------###

## Reproduce the Tests from Lab 1 ###

## t-test:
t.test(mpg ~ am, data = cars, var.equal = TRUE)
summary(lm(mpg ~ am, data = cars))

## Correlation:
with(cars, cor.test(mpg, wt))

## A naive approach:
summary(lm(mpg ~ wt, data = cars))

## We need to standardize the regression coefficients:
summary(lm(scale(mpg) ~ scale(wt), data = cars))

###--------------------------------------------------------------------------###

### Multiple Linear Regression ###

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
se4 <- sqrt(diag(vcov(out4)))
t4  <- cf4 / se4

s4
t4

###--------------------------------------------------------------------------###

### Model Comparison ###

## Change in R^2:
s4$r.squared - s1$r.squared

## Significant increase in R^2?
anova(out1, out4)

## Compare MSE values:
mse1 <- MSE(y_pred = predict(out1), y_true = cars$qsec)
mse4 <- MSE(y_pred = predict(out4), y_true = cars$qsec)

mse1
mse4

###--------------------------------------------------------------------------###

### A More Elegant Way to Build Models ###

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

