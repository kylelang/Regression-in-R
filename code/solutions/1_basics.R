### Title:    Suggested Solutions 1: Basics
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2023-01-26

library(MLmetrics)
source("code/support/helper_functions.R")


###-Data I/O-----------------------------------------------------------------###

## 1.1) Use the readRDS() function to load the "longley.rds" data.

dataDir <- "data/"
fn1     <- "longley.rds"
longley <- readRDS(paste0(dataDir, fn1))


###-Simple Linear Regression-------------------------------------------------###

## 1.2a) Regress "GNP" onto "Year".

out1 <- lm(GNP ~ Year, data = longley)

## Check the summary to make sure nothing broke:
summary(out1)

## 1.2b) What is the slope of "Year" on "GNP"?

coef(out1)["Year"]

## 1.2c) Is the effect of "Year" on "GNP" statistically significant at the
##       alpha = 0.05 level?

isSig(out1, "Year")

## 1.2d) Is the effect of "Year" on "GNP" statistically significant at the
##       alpha = 0.01 level?

isSig(out1, "Year", 0.01)


###-Multiple Linear Regression-----------------------------------------------###

## 1.3a) Regress "GNP" onto "Year" and "Population".

out2 <- lm(GNP ~ Year + Population, data = longley)

## OR ##

out2 <- update(out1, ". ~ . + Population")

summary(out2)

## 1.3b) Is there a significant effect (at the alpha = 0.05 level) of "Year" on
##       "GNP", after controlling for "Population"?

isSig(out2, "Year")

## 1.3c) Is there a significant effect (at the alpha = 0.01 level) of "Year" on
##       "GNP", after controlling for "Population"?

isSig(out2, "Year", 0.01)

## 1.3d) What is the 95% confidence interval (CI) for the partial effect of
##       "Population" on "GNP"?

confint(out2)["Population", ]

## OR ##

confint(out2, "Population")

###--------------------------------------------------------------------------###

## 1.4a) Regress "GNP" onto "Year" and "Employed".

out3 <- lm(GNP ~ Year + Employed, data = longley)

## OR ##

out3 <- update(out1, ". ~ . + Employed")

summary(out3)

## 1.4b) What is the partial effect of "Employed" on "GNP", after controlling
##       for "Year"?

coef(out3)["Employed"]

## 1.4c) Is the partial effect of "Employed" on "GNP" statistically significant
##       at the alpha = 0.05 level?

isSig(out3, "Employed")

## 1.4d) What is the 99% CI for the partial effect of "Employed" on "GNP"?

confint(out3, parm = "Employed", level = 0.99)


###-Model Comparison---------------------------------------------------------###

## 1.5a) What is the difference in R-squared between the simple linear
##       regression of "GNP" onto "Year" and the multiple linear regression of
##       "GNP" onto "Year" and "Population"?

summary(out2)$r.squared - summary(out1)$r.squared

## 1.5b) Is this increase in R-squared significantly different from zero at the
##       alpha = 0.05 level?

av1 <- anova(out1, out2)
av1

isSig(av1)

## 1.5c) What is the value of the test statistic that you used to answer (b)?

av1[2, "F"]

###--------------------------------------------------------------------------###

## 1.6a) What is the MSE for the model that regresses "GNP" onto "Year" and
##      "Population"?

mse <- c()

mse["population"] <- MSE(y_pred = predict(out2), y_true = longley$GNP)
mse["population"]

## 1.6b) What is the MSE for the model that regresses "GNP" onto "Year" and
##       "Employed"?

mse["employed"] <- MSE(y_pred = predict(out3), y_true = longley$GNP)
mse["employed"]

## 1.6c) According to the MSE values calculated above, is "Population" or
##       "Employed" the better predictor of "GNP"?

names(which.min(mse))

## 1.6d) Could we do the comparison in (c) using an F-test for the difference in
##       R-squared values? Why or why not?

### NO. THE COMPARED MODELS ARE NOT NESTED, SO WE CANNOT COMPARE THEM WITH A
### CHANGE IN R^2 TEST.


###-END----------------------------------------------------------------------###

