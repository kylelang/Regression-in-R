### Title:    Regression in R: Lab 5 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-10-08
### Modified: 2022-01-30

rm(list = ls(all = TRUE))

library(car)      # For partial residual plots
library(dplyr)    # For data manipulation
library(sandwich) # For robust standard errors
library(lmtest)   # For hypothesis tests using robust standard errors

## Load some data:
data(Cars93, package = "MASS")


###-Diagnostics--------------------------------------------------------------###

out1 <- lm(Price ~ Horsepower + MPG.city + Passengers, data = Cars93)
summary(out1)

## Residuals vs. Predicted plot:
plot(out1, which = 1)

## Q-Q Plot:
plot(out1, which = 2)

## Scale-Location Plot:
plot(out1, which = 3)

## Cook's Distance Plot:
plot(out1, which = 4)

## Residuals vs. Leverage Plot:
plot(out1, which = 5)

## Panel of partial residual plots:
crPlots(out1)

## Invidual partial residual plots:
crPlot(out1, "Horsepower")
crPlot(out1, "MPG.city")
crPlot(out1, "Passengers")


###-Robust Standard Errors---------------------------------------------------###

## Use sandwich::vcovHC() to compute the HC estimate of the ACOV:
covHC1 <- vcovHC(out1)

## Do a robust test of the coefficients:
coeftest(out1, vcov = covHC1)

## Compare the default version:
summary(out1)$coefficients

## Extend 'out1' by adding some interactions:
out1.2 <- update(out1, ". ~ . + Horsepower * MPG.city + Passengers * MPG.city")
summary(out1.2)

## Compare the 'out1' and 'out1.2' models using robust SEs:
waldtest(out1, out1.2, vcov = vcovHC)

### OR ###

covHC1.2 <- vcovHC(out1.2)
waldtest(out1, out1.2, vcov = covHC1.2)


###-Another Model------------------------------------------------------------###

out2 <- lm(MPG.city ~ Horsepower + Fuel.tank.capacity + Weight, data = Cars93)
summary(out2)

## Residuals vs. Predicted plot:
plot(out2, which = 1)

## Q-Q Plot:
plot(out2, which = 2)

## Scale-Location Plot:
plot(out2, which = 3)

## Cook's Distance Plot:
plot(out2, which = 4)

## Residuals vs. Leverage Plot:
plot(out2, which = 5)

## Partial Residual Plots:
crPlots(out2)


###-Outliers & High-Leverage Cases-------------------------------------------###

## Externally studentized residuals to check for outliers:
(sr2 <- rstudent(out2))

## Create index plot of studentized residuals:
plot(sr2)

## Find the observation number for the two most extreme outliers:
badSr2 <- sr2 %>% abs() %>% sort() %>% tail(2) %>% names() %>% as.numeric()
badSr2

## Compute leverages to find high-leverage points:
lev2 <- hatvalues(out2)
lev2

## Create index plot of leverages:
plot(lev2)

## Store the observation numbers for the most extreme leverages:
badLev2 <- lev2 %>% sort() %>% tail(3) %>% names() %>% as.numeric()
badLev2


###-Measures of Influence----------------------------------------------------###

## Compute a panel of leave-one-out influence measures:
(im2 <- influence.measures(out2))

## Compute individual measures of influence:
(cd2  <- cooks.distance(out2))
(dfb2 <- dfbetas(out2))

## Create index plots for measures of influence:
plot(cd2)

plot(dfb2[ , 1])
plot(dfb2[ , 2])
plot(dfb2[ , 3])
plot(dfb2[ , 4])

## Find the single most influential observation:
(maxCd    <- which.max(cd2))
(maxDfbI  <- which.max(abs(dfb2[ , 1])))
(maxDfbB1 <- which.max(abs(dfb2[ , 2])))
(maxDfbB2 <- which.max(abs(dfb2[ , 3])))
(maxDfbB3 <- which.max(abs(dfb2[ , 4])))


###-Refit the Model----------------------------------------------------------###

## Exclude the most influential observation:
Cars93.2 <- Cars93[-maxCd, ]

## Refit the model:
out2.2 <- update(out2, data = Cars93.2)

summary(out2)
summary(out2.2)


###-END----------------------------------------------------------------------###
