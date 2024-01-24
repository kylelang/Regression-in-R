### Title:    Regression in R 4: Diagnostics
### Author:   Kyle M. Lang
### Created:  2017-10-08
### Modified: 2024-01-24

rm(list = ls(all = TRUE))


###-Preliminaries------------------------------------------------------------###

library(car)      # For partial residual plots
library(dplyr)    # For data manipulation
library(sandwich) # For robust standard errors
library(lmtest)   # For hypothesis tests using robust standard errors

## Load some data:
data(Cars93, package = "MASS")

################################################################################
## PRACTICE PROBLEM 4.1
##
## Use the readRDS() function to load the "airQual.rds" dataset.
##
## NOTE: Unless otherwise instructed, use the airQual data to answer all of the
##       following practice problems.
##
################################################################################


###-Diagnostics--------------------------------------------------------------###

out1 <- lm(Price ~ Horsepower + MPG.city + Passengers, data = Cars93)
summary(out1)

################################################################################
## PRACTICE PROBLEM 4.2
##
## Regress "Temp" onto "Ozone", "Wind", "Solar.R", and the square of "Ozone".
## - In the following practice problems, this model will be referred to as "M0".
##
################################################################################

## Generate all diagnostic plots:
plot(out1)

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

################################################################################
## PRACTICE PROBLEM 4.3
##
## Use the model you estimated in PP 4.2 (i.e., M0) to complete the following
## tasks and answer the associated questions.
##
## a) Plot the residuals from the model against its fitted values.
##    - What can you infer from this plot?
## b) Evaluate the homoscedasticity assumption using a scale-location plot.
##    - What conclusions can you draw from this plot?
## c) Evaluate the linearity assumption using partial residual plots.
##    - What can you infer from these plots?
## d) Evaluate the normality of the residuals using a Q-Q plot.
##    - Judging by the information gained from this Q-Q plot, do you think it's
##      safe to assume normally distributed errors?
##
################################################################################


###-Robust Standard Errors---------------------------------------------------###

## Use sandwich::vcovHC() to compute the HC estimate of the ACOV:
covHC1 <- vcovHC(out1)

## Do a robust test of the coefficients:
coeftest(out1, vcov = covHC1)

## Compare the default version:
summary(out1)$coefficients

################################################################################
## PRACTICE PROBLEM 4.4
##
## a) Estimate the heteroscedasticity consistent (HC) asymptotic covariance
##    matrix for M0 (i.e., the model from PP 4.2).
## b) Use the HC covariance matrix from (a) to test the coefficients of M0 with
##    robust SEs.
## c) Compare the results from (b) to the default tests of M0's coefficients.
##    - What changes when using robust SEs?
##
################################################################################

## Extend 'out1' by adding some interactions:
out1.2 <- update(out1, ". ~ . + Horsepower * MPG.city + Passengers * MPG.city")
summary(out1.2)

## Compare the 'out1' and 'out1.2' models using robust SEs:
waldtest(out1, out1.2, vcov = vcovHC)

### OR ###

covHC1.2 <- vcovHC(out1.2)
waldtest(out1, out1.2, vcov = covHC1.2)

################################################################################
## PRACTICE PROBLEM 4.5
##
## Update M0 by adding the squares of "Wind" and "Solar.R" and re-estimating the
## model.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 4.6
##
## Use the models you estimated in PP 4.2 and PP 5.5 to complete the following
## tasks.
##
## a) Using HC estimates of the SEs, conduct a nested model comparison to test
##    if adding the squares of "Wind" and "Solar.R" to M0 explains
##    significantly more variance in "Temp".
##    - What is the conclusion of this test?
## b) Compare the test in (a) to the default version that does not use HC
##    estimates of the SEs.
##    - What differs when using robust SEs?
##
################################################################################


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


################################################################################
## PRACTICE PROBLEM 4.7
##
## a) Compute the studentized residuals of M0 (i.e., the model from PP 4.2).
## b) Create an index plot of the studentized residuals computed in (a).
##    - What can you infer from this plot?
## c) What are the observation numbers for the two most probable outliers
##    according to the studentized residuals from (a)?
##
################################################################################

## Compute leverages to find high-leverage points:
lev2 <- hatvalues(out2)
lev2

## Create index plot of leverages:
plot(lev2)

## Store the observation numbers for the most extreme leverages:
badLev2 <- lev2 %>% sort() %>% tail(3) %>% names() %>% as.numeric()
badLev2

################################################################################
## PRACTICE PROBLEM 4.8
##
## a) Compute the leverages of M0.
## b) Create an index plot of the leverages computed in (a).
##    - What can you infer from this plot?
## c) What are the observation numbers with the three highest leverages?
##
################################################################################


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

################################################################################
## PRACTICE PROBLEM 4.9
##
## a) Compute the Cook's distances for M0.
## b) Create an index plot of the distances computed in (a).
##    - What can you infer from this plot?
## c) What are the observation numbers for the five most influential cases
##    according to the distances from (a)?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 4.10
##
## a) Compute the DFFITS values for M0.
## b) Create an index plot of the DFFITS values computed in (a).
##    - What can you infer from this plot?
## c) What are the observation numbers for the five most influential cases
##    according to the DFFITS values from (a)?
## d) Are the observations flagged in (c) the same as those flagged in PP 4.9c?
##
################################################################################

################################################################################
## PRACTICE PROBLEM 4.11
##
## a) Compute the DFBETAS values for M0.
## b) Create an index plot of the DFBETAS values for the intercept.
##    - What can you infer from this plot?
## c) Create an index plot of the DFBETAS values for the slope of "Wind".
##    - What can you infer from this plot?
##
################################################################################


###-Effects of Influential Observations--------------------------------------###

## Exclude the most influential observation:
Cars93.2 <- Cars93[-maxCd, ]

## Refit the model:
out2.2 <- update(out2, data = Cars93.2)

summary(out2)
summary(out2.2)

################################################################################
## PRACTICE PROBLEM 4.12
##
## a) Remove the five most influential cases flagged in PP 4.9c, and use the
##    cleaned data to rerun M0.
## b) Compare the results of the model estimated in (a) to the results of the
##    original M0 fit to the entire dataset.
##    - What changes when removing the influential cases?
##
################################################################################


###-END----------------------------------------------------------------------###
