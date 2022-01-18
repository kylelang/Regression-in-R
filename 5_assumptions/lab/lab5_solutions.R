### Title:    Regression in R: Lab 5 Suggested Solutions
### Author:   Kyle M. Lang
### Created:  2018-10-09
### Modified: 2022-01-18

###-Overview-----------------------------------------------------------------###

## You will practice regression diagnostics for MLR models.

## You will need the "airQual.rds" dataset which is available in the "data"
## directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) If you have not already done so, use the install.packages() function to
##    install the "lmtest", and "sandwich" packages.

install.packages(c("lmtest", "sandwich"), repos = "http://cloud.r-project.org")

## 2) Use the library() function to load the "lmtest" and "sandwich" packages.

library(lmtest)
library(sandwich)
library(dplyr)    # We'll also use this for some answers

## 3) Use the readRDS() function to load the "airQual.rds" dataset into your
##    workspace.

dataDir <- "../../data/"
airQual <- readRDS(paste0(dataDir, "airQual.rds"))


###-Diagnostics--------------------------------------------------------------###

### Use the "airQual" data to complete the following:

## 1) Regress "Temp" onto "Ozone", "Wind", "Solar.R", and the square of "Ozone".
##    -- In the following sections, this model will be referred to as "M0".

m0 <- lm(Temp ~ Ozone + Wind + Solar.R + I(Ozone^2), data = airQual)
summary(m0)

## 2a) Plot the residuals from the model estimated in (1) against its fitted
##     values.

plot(m0, 1)

## 2b) What can you infer from the plot created in (2a)?

### THE MODEL DOES NOT APPEAR TO BE MISSPECIFIED, BUT THERE MAY BE SOME
### HETEROSCEDASTICITY.

## 3a) Evaluate the homoscedasticity assumption for the model in (1) using a
##     location-scale plot.

plot(m0, 3)

## 3b) What conclusions can you draw from the plot in (3a)?

### THERE APPEARS TO BE SUBSTANTIAL HETEROSCEDASTICITY.

## 4a) Evaluate the linearity assumption for the model in (1) using partial
##     residual plots.

crPlots(m0)

## 4b) What can you infer from the plots in (4a)?

### The "Wind" and "Solar.R" variables seem like they may have nonlinear
### relations with "Temp".

## 5a) Evaluate the normality of the residuals from the model in (1) using a Q-Q
##     plot.

plot(m0, 2)

## 5b) Judging by the information gained in (5a), do you think it's safe to
##     assume normally distributed errors for the model in (1)?

### Not so much. The tails don't follow the ideal line very well.


###-Robust SEs---------------------------------------------------------------###

### Use the "airQual" data to complete the following:

## 1) Estimate the heteroscedasticity consistent (HC) asymptotic covariance
##    matrix for M0 (i.e., the model from [1] in the "Diagnostics" section).

covHC0 <- vcovHC(m0)

## 2a) Use the HC covariance matrix from (1) to test the coefficients of M0 with
##     robust SEs.

coeftest(m0, vcov = covHC0)

## 2b) Compare the results from (2a) to the default tests of M0's coefficients.
##     What changes when using robust SEs?

summary(m0)$coefficients

### THE ROBUST STANDARD ERRORS ARE A LITTLE BIT LARGER THAN THEIR DEFAULT
### COUNTERPARTS.

## 3) Update M0 by adding the squares of "Wind" and "Solar.R".

m0.2 <- update(m0, ". ~ . + I(Wind^2) + I(Solar.R^2)")
summary(m0.2)

## 4a) Using HC estimates of the SEs, conduct a nested model comparison to test
##     if adding the squares of "Wind" and "Solar.R" to M0 explains
##     significantly more variance in "Temp".

waldtest(m0, m0.2, vcov = vcovHC)

## 4b) What is the conclusion of the test you conducted in (4a)?

### ADDING THE TWO NEW TERMS DOES NOT EXPLAIN A SIGNIFICANTLY GREATER
### PROPORTION OF VARIABILITY IN "TEMP".

## 4c) Compare the test in (4a) to the default version that does not use HC
##     estimates of the SEs. What differs when using robust SEs?

anova(m0, m0.2)

### THE ROBUST VERSION PRODUCES A LARGER TEST STATISTIC.


###-Influential Observations-------------------------------------------------###

### Use the "airQual" data to complete the following:

## 1a) Compute the studentized residuals of M0 (i.e., the model from [1] in the
##     "Diagnostics" section).

sr0 <- rstudent(m0)

## 1b) Create an index plot of the residuals computed in (1a).

plot(sr0)

## 1c) What can you infer from the plot in (1b)?

### TWO OBSERVATIONS APPEAR TO BE POTENTIAL OUTLIERS (I.E., THEY HAVE
### ABSOLUTE STUDENTIZED RESIDUALS LARGER THAN 3).

## 1d) What are the observation numbers for the two most probable outliers
##     according to the residuals from (1a)?

badSr <- sr0 %>% abs() %>% sort() %>% tail(2) %>% names() %>% as.numeric()
badSr

## 2a) Compute the leverages of M0.

lv0 <- hatvalues(m0)

## 2b) Create an index plot of the leverages computed in (2a).

plot(lv0)

## 2c) What can you infer from the plot in (2b)?

### THREE OBSERVATIONS SEEM TO HAVE ESPECIALLY HIGH LEVERAGES.

## 2d) What are the observation numbers with the three highest leverages?

badLv <- lv0 %>% sort() %>% tail(3) %>% names() %>% as.numeric()
badLv

## 3a) Compute the Cook's distances of M0.

cd0 <- cooks.distance(m0)

## 3b) Create an index plot of the distances computed in (3a).

plot(cd0)

## 3c) What can you infer from the plot in (3b)?

### FIVE OBSERVATIONS APPEAR TO BE ESPECIALLY INFLUENTIAL.

## 3d) What are the observation numbers for the five most influential cases
##     according to the distances from (3a)?

badCd <- cd0 %>% sort() %>% tail(5) %>% names() %>% as.numeric()
badCd

## 4a) Remove the five most influential cases from (3d), and use the cleaned
##     data to rerun M0.

m0.3 <- update(m0, data = airQual[-badCd, ])
summary(m0.3)

## 4b) Compare the results of the model in (4a) to the results of the original
##     M0. What changes when removing the influential cases?

summary(m0)

### THE R-SQUARED FROM THE CLEANED MODEL IS LARGER AS ARE ALL OF THE SLOPES.


###-END----------------------------------------------------------------------###
