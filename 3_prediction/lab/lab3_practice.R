### Title:    Regression in R: Lab 3 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2022-01-30


###-Overview-----------------------------------------------------------------###

## You will practice prediction and cross-validation.

## You will need the "yps.rds" dataset to answer the following questions. This
## dataset is available in the "data" directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) Use the library() function to load the "MLmetrics" and "DAAG" packages.

## 2) Use the readRDS() function to load the "yps.rds" dataset.

## 3) Use the set.seed() function to set the random number seed.


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


###-Prediction/K-Fold Cross-Validation---------------------------------------###

### Use the "yps" data to complete the following:

## 1) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

## 2) Use the training data from (1) to run 5-fold cross-validation comparing
##    the following three models:
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Keeping.promises", "Empathy", "Friends.versus.money", and "Charity"
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Branded.clothing", "Entertainment.spending", "Spending.on.looks", and
##       "Spending.on.gadgets"
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Workaholism", "Reliability", "Responding.to.a.serious.letter", and
##       "Assertiveness"

## 3a) When comparing the models you tested in (2) based on their relative
##     cross-validation errors, which model should be preferred?
## 3b) Does the result from (3a) agree with the results from (3f) and (6c) in
##     the "Prediction/Split-Sample Cross-Validation" section?
## 3c) Use the testing data that you set aside in (1) to estimate the prediction
##     error (i.e., test set MSE) of the model chosen in (3a).


###-END----------------------------------------------------------------------###
