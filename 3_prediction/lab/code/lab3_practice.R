### Title:    Stats & Methods Lab 3 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2020-09-16


###          ###
### Overview ###
###          ###

## You will practice prediction, cross-validation, and multiple imputation.

## You will need the "yps.rds" and "bfiANC2.rds" datasets to answer the
## following questions. These datasets are available in the "data" directory for
## this lab.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "library" function to load the "MLmetrics" and "mice" packages.

## 2) Use the "source" function to source the "studentFunctions.R" script.

## 3) Use the "paste0" function and the "readRDS" function to load the "yps.rds"
##    and "bfiANC2.rds" datasets into your workspace.

## 4) Use the "set.seed" function to set the random number seed.


##--Prediction/Split-Sample Cross-Validation----------------------------------##

### Use the "yps" data to complete the following:

## 1) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

## 2a) Use the training data to estimate a baseline model that regresses
##     "Number.of.friends onto "Age" and "Gender".
## 2b) Update the baseline model (from 2a) by adding "Keeping.promises",
##     "Empathy", "Friends.versus.money", and "Charity" as additional
##     predictors.
## 2c) Update the baseline model (from 2a) by adding "Branded.clothing",
##     "Entertainment.spending", "Spending.on.looks", and "Spending.on.gadgets"
##     as additional predictors.
## 2d) Update the baseline model (from 2a) by adding "Workaholism",
##     "Reliability", "Responding.to.a.serious.letter", and "Assertiveness" as
##     additional predictors.

## 3a) Compute training-set predictions from the three models you estimated in
##     (2b), (2c), and (2d).
## 3b) Compute training-set MSEs for the three models you estimated in (2b),
##     (2c), and (2d).
## 3c) Compute test-set predictions from the three models you estimated in (2b),
##     (2c), and (2d).
## 3d) Compute test-set MSEs for the three models you estimated in (2b), (2c),
##     and (2d).
## 3e) When comparing the models you estimated in (2b), (2c), and (2d) based on
##     their relative training-set prediction errors, which model should be
##     preferred?
## 3f) When comparing the models you estimated in (2b), (2c), and (2d) based on
##     their relative test-set prediction errors, which model should be
##     preferred?

## 4) Randomly split the sample into disjoint training, validation, and testing
##    sets with sample sizes of N = 700, N = 155, and N = 155, respectively.

## 5a) Use the training data from (4) to re-estimate the model from (2b).
## 5b) Use the training data from (4) to re-estimate the model from (2c).
## 5c) Use the training data from (4) to re-estimate the model from (2d).

## 6a) Compute the validation-set predictions from the three models you
##     estimated in (5a), (5b), and (5c).
## 6b) Compute the validation-set MSEs for the three models you estimated in
##     (5a), (5b), and (5c).
## 6c) When comparing the models you estimated in (5a), (5b), and (5c) based on
##     their relative prediction errors, which model should be preferred?

## 7) Use the testing data that you set aside in (4) to estimate the prediction
##    error (i.e., test set MSE) of the model chosen in (6c).


##--Prediction/K-Fold Cross-Validation----------------------------------------##

### Use the "yps" data to complete the following:

## 1) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

## 2) Use the training data from (1) to run 5-fold cross-validation comparing
##    the following three models:
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Keeping.promises", "Empathy", "Friends.versus.money", and "Charity".
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Branded.clothing", "Entertainment.spending", "Spending.on.looks", and
##       "Spending.on.gadgets".
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Workaholism", "Reliability", "Responding.to.a.serious.letter", and
##       "Assertiveness".

## 3a) When comparing the models you tested in (2) based on their relative
##     cross-validation errors, which model should be preferred?
## 3b) Does the result from (3a) agree with the results from (3f) and (6c) in
##     the Lab 3 Practice Script?
## 3c) Use the testing data that you set aside in (1) to estimate the prediction
##     error (i.e., test set MSE) of the model chosen in (3a).


##--Multiple Imputation-------------------------------------------------------##

### Use the "bfiANC2" data for the following:

## 1) Use the "mice" package, with the following setup, to create multiple
##    imputations of the "bfiANC2" data.
##    -- 25 imputations
##    -- 15 iterations
##    -- A random number seed of "314159"
##    -- All "A" variables imputed with predictive mean matching
##    -- All "N" variables imputed with Bayesian linear regression
##    -- All "C" variables imputed with linear regression using bootstrapping
##    -- The "education" variable imputed with polytomous logistic regression
##    -- A predictor matrix generated with the "quickpred" function using the
##       following setup:
##    ---- The minimum correlation set to 0.25
##    ---- The "age" and "gender" variables included in all elementary
##         imputation models
##    ---- The "id" variable excluded from all elementary imputation models

## 2a) Create traceplots of the imputed values' means and SDs to check that the
##     imputation models converged.
## 2b) Create overlaid density plots of the imputed vs. observed values to
##     sanity-check the imputaitons.
## 2c) Based on the plots you created in (2a) and (2b), would you say that the
##     imputations are valid?

## 3a) Use the "lm.mids" function to regress "A1" onto "C1", "N1", and
##     "education" using the multiply imputed data from (1)
## 3b) Use the "lm.mids" function to regress "A1" onto "C1", "N1", "education",
##     "age", and "gender" using the multiply imputed data from (1)

## 4a) What is the MI estimate of the slope of "age" on "A1" from (3b)?
## 4b) Is the effect in (4a) significant at the alpha = 0.05 level?

## 5a) What is the MI estimate of the slope of "N1" on "A1" from (3a)?
## 5b) Is the effect in (5a) significant at the alpha = 0.01 level?

## 6a) What is the MI estimate of the R^2 from the model in (3a)?
## 6b) What is the MI estimate of the R^2 from the model in (3b)?

## 7a) What is the MI estimate of the increase in R^2 when going from the model
##     in (3a) to the model in (3b)?
## 7b) Is the increase in R^2 from (7a) statistically significant at the
##     alpha = 0.05 level?
## 7c) What is the value of the test statistic that you used to answer (7b)?
