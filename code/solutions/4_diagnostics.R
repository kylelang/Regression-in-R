### Title:    Suggested Solutions 4: Diagnostics
### Author:   Kyle M. Lang
### Created:  2018-10-09
### Modified: 2024-01-24

library(car)      
library(dplyr)    
library(sandwich) 
library(lmtest)   

###-Preliminaries------------------------------------------------------------###

## 4.1) Use the readRDS() function to load the "airQual.rds" dataset.

dataDir <- "data/"
airQual <- readRDS(paste0(dataDir, "airQual.rds"))


###-Diagnostics--------------------------------------------------------------###

## 4.2) Regress "Temp" onto "Ozone", "Wind", "Solar.R", and the square of
##      "Ozone".

m0 <- lm(Temp ~ Ozone + Wind + Solar.R + I(Ozone^2), data = airQual)
summary(m0)

###--------------------------------------------------------------------------###

## 4.3a) Plot the residuals from the model against its fitted values.
##       - What can you infer from this plot?

plot(m0, 1)

### THE MODEL DOES NOT APPEAR TO BE MISSPECIFIED, BUT THERE MAY BE SOME
### HETEROSCEDASTICITY.

## 4.3b) Evaluate the homoscedasticity assumption using a scale-location plot.
##       - What conclusions can you draw from this plot?

plot(m0, 3)

### THERE APPEARS TO BE SUBSTANTIAL HETEROSCEDASTICITY.

## 4.3c) Evaluate the linearity assumption using partial residual plots.
##       - What can you infer from these plots?

crPlots(m0)

### The "Wind" and "Solar.R" variables seem like they may have nonlinear
### relations with "Temp".

## 4.3d) Evaluate the normality of the residuals using a Q-Q plot.
##       - Judging by the information gained from this Q-Q plot, do you think
##         it's safe to assume normally distributed errors?

plot(m0, 2)

### Not so much. The tails don't follow the ideal line very well.


###-Robust SEs---------------------------------------------------------------###

### Use the "airQual" data to complete the following:

## 4.4a) Estimate the heteroscedasticity consistent (HC) asymptotic covariance
##       matrix for M0 (i.e., the model from PP 4.2).

covHC0 <- vcovHC(m0)

## 4.4b) Use the HC covariance matrix from (a) to test the coefficients of M0
##       with robust SEs.

coeftest(m0, vcov = covHC0)

## 4.4c) Compare the results from (b) to the default tests of M0's coefficients.
##    - What changes when using robust SEs?

summary(m0)$coefficients

### THE ROBUST STANDARD ERRORS ARE A LITTLE BIT LARGER THAN THEIR DEFAULT
### COUNTERPARTS.

###--------------------------------------------------------------------------###

## 4.5) Update M0 by adding the squares of "Wind" and "Solar.R" and
##      re-estimating the model.

m0.1 <- update(m0, ". ~ . + I(Wind^2) + I(Solar.R^2)")
summary(m0.1)

###--------------------------------------------------------------------------###

## 4.6a) Using HC estimates of the SEs, conduct a nested model comparison to
##       test if adding the squares of "Wind" and "Solar.R" to M0 explains
##       significantly more variance in "Temp".
##       - What is the conclusion of this test?

waldtest(m0, m0.1, vcov = vcovHC)

### ADDING THE TWO NEW TERMS DOES NOT EXPLAIN A SIGNIFICANTLY GREATER
### PROPORTION OF VARIABILITY IN "TEMP".

## 4.6b) Compare the test in (a) to the default version that does not use HC
##       estimates of the SEs.
##       - What differs when using robust SEs?

anova(m0, m0.1)

### THE ROBUST VERSION PRODUCES A LARGER TEST STATISTIC.


###-Outliers & High-Leverage Cases-------------------------------------------###

## 4.7a) Compute the studentized residuals of M0 (i.e., the model from PP 5.2).

sr0 <- rstudent(m0)

## 4.7b) Create an index plot of the studentized residuals computed in (a).
##       - What can you infer from this plot?

plot(sr0)

### TWO OBSERVATIONS APPEAR TO BE POTENTIAL OUTLIERS (I.E., THEY HAVE
### ABSOLUTE STUDENTIZED RESIDUALS LARGER THAN 3).

## 4.7c) What are the observation numbers for the two most probable outliers
##       according to the studentized residuals from (a)?

sr0 %>% abs() %>% sort() %>% tail(2) %>% names() %>% as.numeric()

###--------------------------------------------------------------------------###

## 4.8a) Compute the leverages of M0.

lv0 <- hatvalues(m0)

## 4.8b) Create an index plot of the leverages computed in (a).
##       - What can you infer from this plot?

plot(lv0)

### THREE OBSERVATIONS SEEM TO HAVE ESPECIALLY HIGH LEVERAGES.

## 4.8c) What are the observation numbers with the three highest leverages?

lv0 %>% sort() %>% tail(3) %>% names() %>% as.numeric()


###-Measures of Influence----------------------------------------------------###

## 4.9a) Compute the Cook's distances for M0.

cd0 <- cooks.distance(m0)

## 4.9b) Create an index plot of the distances computed in (a).
##       - What can you infer from this plot?

plot(cd0)

### FIVE OBSERVATIONS APPEAR TO BE ESPECIALLY INFLUENTIAL.

## 4.9c) What are the observation numbers for the five most influential cases
##       according to the distances from (a)?

badCd <- cd0 %>% sort() %>% tail(5) %>% names() %>% as.numeric()
badCd

###--------------------------------------------------------------------------###

## 4.10a) Compute the DFFITS values for M0.

dff0 <- dffits(m0)

## 4.10b) Create an index plot of the DFFFITS values in (a).
##        - What can you infer from this plot?

plot(dff0)

### FIVE OBSERVATIONS APPEAR TO BE ESPECIALLY INFLUENTIAL.

## 4.10c) What are the observation numbers for the five most influential cases
##        according to the DFFITS values from (a)?

badDff <- dff0 %>% abs() %>% sort() %>% tail(5) %>% names() %>% as.numeric()
badDff

## 4.10d) Are the observations flagged in (c) the same as those flagged in
##        PP 4.9c?

all.equal(badCd, badDff) %>% ifelse("Yes", "No")

###--------------------------------------------------------------------------###

## 4.11a) Compute the DFBETAS values for M0.

dfb0 <- dfbetas(m0)

## 4.11b) Create an index plot of the DFBETAS values for the intercept.
##        - What can you infer from this plot?

plot(dfb0[ , 1])

## 4.11c) Create an index plot of the DFBETAS values for the slope of "Wind".
##        - What can you infer from this plot?

plot(dfb0[ , 3])


###-Effects of Influential Observations--------------------------------------###

## 4.12a) Remove the five most influential cases flagged in PP 5.9c, and use the
##    cleaned data to rerun M0.

m0.2 <- update(m0, data = airQual[-badCd, ])
summary(m0.2)

## 4.12b) Compare the results of the model estimated in (a) to the results of
##        the original M0 fit to the entire dataset.
##        - What changes when removing the influential cases?

summary(m0)

### THE R-SQUARED FROM THE CLEANED MODEL IS LARGER AS ARE ALL OF THE SLOPES.


###-END----------------------------------------------------------------------###
