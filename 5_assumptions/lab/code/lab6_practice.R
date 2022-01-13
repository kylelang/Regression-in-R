### Title:    Stats & Methods Lab 6 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-10-09
### Modified: 2020-10-09


###          ###
### Overview ###
###          ###

## You will practice regression diagnostics for MLR models.

## You will need the "airQual.rds" dataset which is available in the "data"
## directory for this lab.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "install.packages" function to install the "moments", "lmtest",
##    and "sandwich" packages.

## 2) Use the "library" function to load the "moments", "lmtest", and "sandwich"
##    packages.

## 3) Use the "readRDS" function to load the "airQual.rds" dataset into your
##    workspace.


##--Model specification-------------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1) Regress "Temp" onto "Ozone", "Wind", and "Solar.R".

## 2a) Plot the residuals from the model estimated in (1) against its fitted
##     values.
## 2b) Add a loess line to the residual plot from (2a).
## 2c) What can you infer from the plots created in (2a) and (2b)?
## 2d) What do you think is the best course of action to correct the issues
##     represented in the plot from (2a)?

## 3a) Conduct a Ramsey RESET for the model estimated in (1).
##     -- Add the second and third powers of the fitted values.
## 3b) What do the results of the RESET in (3a) tell you?

## 4a) Update the model estimated in (1) three times. In each new model, add the
##     square of exactly one of the predictor variables.
##     -- Each of these three models should be identical to the model from (1)
##        except for the inclusion of a different quadratic term.
## 4b) For each of the updated models estimated in (4a) compute the same type of
##     residual plot that you created in (2a) and conduct a Ramsey RESET as you
##     did in (3a).
## 4c) Which predictor's quadratic term most improved the model specification?
## 4d) Does the RESET for the model you indicated in (4c) still suggest
##     significant misspecification?


##--Diagnostics---------------------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1) Regress "Temp" onto "Ozone", "Wind", "Solar.R", and the square of "Ozone".
##    -- In the following sections, this model will be referred to as "M0".

## 2a) Plot the residuals from the model estimated in (1) against its fitted
##     values, and add a loess line to the plot.
## 2b) What can you infer from the plot created in (2a)?

## 4a) Conduct a Breusch-Pagan test for the model estimated in (1).
## 4b) What does the Breusch-Pagan test you conducted in (4a) tell you?
## 4c) Do the Breusch-Pagan test and the residual plots from (2a) and (2b) agree?

## 5a) Evaluate the normality of the residuals from the model in (1) using a Q-Q
##     plot, the skewness, the kurtosis, the Shapiro-Wilk test, and the
##     Kolmogorov-Smirnov test.
## 5b) Do the results of the diagnostics you conducted for (5a) agree?
## 5c) Create a kernel density plot of the residuals from the model in (1).
## 5d) Judging by the information gained in (5a) and (5b), do you think it's
##     safe to assume normally distributed errors for the model in (1)?


##--Robust SEs----------------------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1) Estimate the heteroscedasticity consistent (HC) asymptotic covariance
##    matrix for M0 (i.e., the model from [1] in the "Diagnostics" section).

## 2a) Use the HC covariance matrix from (1) to test the coefficients of M0 with
##     robust SEs.
## 2b) Compare the results from (2a) to the default tests of M0's coefficients.
##     What changes when using robust SEs?

## 3) Update M0 by adding the squares of "Wind" and "Solar.R".

## 4a) Using HC estimates of the SEs, conduct a nested model comparison to test
##     if adding the squares of "Wind" and "Solar.R" to M0 explains
##     significantly more variance in "Temp".
## 4b) What is the conclusion of the test you conducted in (4a)?
## 4c) Compare the test in (4a) to the default version that does not use HC
##     estimates of the SEs. What differs when using robust SEs?


##--Influential observations--------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1a) Compute the studentized residuals of M0 (i.e., the model from [1] in the
##     "Diagnostics" section).
## 1b) Create an index plot of the residuals computed in (1a).
## 1c) What can you infer from the plot in (1b)?
## 1d) What are the observation numbers for the two most probable outliers
##     according to the residuals from (1a)?

## 2a) Compute the leverages of M0.
## 2b) Create an index plot of the leverages computed in (2a).
## 2c) What can you infer from the plot in (2b)?
## 2d) What are the observation numbers with the three highest leverages?

## 3a) Compute the Cook's distances of M0.
## 3b) Create an index plot of the distances computed in (3a).
## 3c) What can you infer from the plot in (3b)?
## 3d) What are the observation numbers for the five most influential cases
##     according to the distances from (3a)?

## 4a) Compute the DFFITS of M0.
## 4b) Create an index plot of the DFFITS computed in (4a).
## 4c) What can you infer from the plot in (4b)?
## 4d) What are the observation numbers for the five most influential cases
##     according to the DFFITS from (4a)?
## 4e) Do the results from (4d) agree with the results from (3d)?
## 4e) What do you notice about the set of observations flagged as influential
##     cases in (4d) relative to the observations flagged as high leverage
##     points in (2d) and those flagged as outliers in (1d)?

## 5a) Remove the five most influential cases from (4d), and use the cleaned
##     data to rerun M0.
## 5b) Compare the results of the model in (5a) to the results of the original
##     M0. What changes when removing the influential cases?

##----------------------------------------------------------------------------##
