### Title:    Regression in R: Lab 5 Practice Script
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

## 2) Use the library() function to load the "lmtest", and "sandwich" packages.

## 3) Use the readRDS() function to load the "airQual.rds" dataset into your
##    workspace.


###-Diagnostics--------------------------------------------------------------###

### Use the "airQual" data to complete the following:

## 1) Regress "Temp" onto "Ozone", "Wind", "Solar.R", and the square of "Ozone".
##    -- In the following sections, this model will be referred to as "M0".

## 2a) Plot the residuals from the model estimated in (1) against its fitted
##     values.
## 2b) What can you infer from the plot created in (2a)?

## 3a) Evaluate the homoscedasticity assumption for the model in (1) using a
##     location-scale plot.
## 3b) What conclusions can you draw from the plot in (3a)?

## 4a) Evaluate the linearity assumption for the model in (1) using partial
##     residual plots.
## 4b) What can you infer from the plots in (4a)?

## 5a) Evaluate the normality of the residuals from the model in (1) using a Q-Q
##     plot.
## 5b) Judging by the information gained in (5a), do you think it's safe to
##     assume normally distributed errors for the model in (1)?


###-Robust SEs---------------------------------------------------------------###

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


###-Influential observations-------------------------------------------------###

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

## 4a) Remove the five most influential cases flagged in (3d), and use the
##     cleaned data to rerun M0.
## 4b) Compare the results of the model in (4a) to the results of the original
##     M0. What changes when removing the influential cases?

###-END----------------------------------------------------------------------###
