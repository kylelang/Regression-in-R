### Title:    Regression in R 3: Prediction
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2023-01-26


###-Preliminaries------------------------------------------------------------###

rm(list = ls(all = TRUE)) # Clear workspace

set.seed(235711) # Set the random number seed

library(MLmetrics) # For MSEs
library(dplyr)     # For pipes
library(DAAG)      # For cross-validation


###-Data I/O-----------------------------------------------------------------###

dataDir <- "data/"
ins     <- read.csv(paste0(dataDir, "insurance.csv"))

################################################################################
## PRACTICE PROBLEM 3.1
##
## Use the readRDS() function to load the "yps.rds" dataset.
##
## NOTE: Unless otherwise specified, use these data to answer all of the
##       following practice problems.
##
################################################################################


###-Prediction & Simple Split-Sample Cross-Validation------------------------###

## Split the data into training and testing sets:
ind   <- sample(1:nrow(ins))
train <- ins[ind[1:1000], ]
test  <- ins[ind[1001:nrow(ins)], ]

################################################################################
## PRACTICE PROBLEM 3.2
##
## Randomly split the sample into disjoint training and testing sets with sample
## sizes of N = 800 and N = 210, respectively.
##
################################################################################

## Estimate three models:
out1 <- lm(charges ~ age + sex, data = train)
out2 <- update(out1, ". ~ . + region + children")
out3 <- update(out2, ". ~ . + bmi + smoker")

## Check that everything worked:
summary(out1)
summary(out2)
summary(out3)

## Generate training-set predictions (i.e., y-hats):
p1 <- predict(out1)
p2 <- predict(out2)
p3 <- predict(out3)

## Generate training-set MSEs:
MSE(y_pred = p1, y_true = train$charges)
MSE(y_pred = p2, y_true = train$charges)
MSE(y_pred = p3, y_true = train$charges)

## Generate test-set predictions:
p1.2 <- predict(out1, newdata = test)
p2.2 <- predict(out2, newdata = test)
p3.2 <- predict(out3, newdata = test)

## Generate test-set MSEs:
MSE(y_pred = p1.2, y_true = test$charges)
MSE(y_pred = p2.2, y_true = test$charges)
MSE(y_pred = p3.2, y_true = test$charges)

## Get confidence and prediction intervals:
p3.3 <- predict(out3, newdata = test, interval = "confidence")
p3.4 <- predict(out3, newdata = test, interval = "prediction")

head(p3.3)
head(p3.4)

################################################################################
## PRACTICE PROBLEM 3.3
##
## a) Use the training data from PP 3.2 to estimate a baseline model that
##    regresses "Number.of.friends onto "Age" and "Gender".
## b) Update the baseline model by adding "Keeping.promises", "Empathy",
##    "Friends.versus.money", and "Charity" as additional predictors.
## c) Update the baseline model by adding "Branded.clothing",
##    "Entertainment.spending", "Spending.on.looks", and "Spending.on.gadgets"
##     as additional predictors.
## d) Update the baseline model by adding "Workaholism", "Reliability",
##    "Responding.to.a.serious.letter", and "Assertiveness" as additional
##    predictors.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.4
##
## Using the data from PP 3.2, complete the following tasks for each of the
## three models you estimated in PP 3.3b, PP 3.3c, and PP 3.3d.
##
## a) Compute training-set predictions.
## b) Compute training-set MSEs.
## c) Compute test-set predictions.
## d) Compute test-set MSEs.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.5
##
## a) When comparing the models you estimated in PP 3.3b, PP 3.3c, and PP 3.3d
##    based on their relative training-set prediction errors, which model should
##    be preferred?
## b) When comparing the models you estimated in PP 3.3b, PP 3.3c, and PP 3.3d
##    based on their relative test-set prediction errors, which model should be
##    preferred?
##
################################################################################


###-Three-Way Split----------------------------------------------------------###

## Split the sample into training, validation, and testing sets:
ind <- c(rep("train", 800),
         rep("valid", 300),
         rep("test", nrow(ins) - 1100)
         ) %>%
    sample()

table(ind)

ins2 <- split(ins, ind)
class(ins2)
ls(ins2)

################################################################################
## PRACTICE PROBLEM 3.6
##
## Randomly split the sample into disjoint training, validation, and testing
## sets with sample sizes of N = 700, N = 155, and N = 155, respectively.
##
################################################################################

## Fit four competing models:
fits <- list()
out0 <- lm(charges ~ age + sex, data = ins2$train)

fits[[1]] <- update(out0, ". ~ . + children")
fits[[2]] <- update(out0, ". ~ . + region")
fits[[3]] <- update(out0, ". ~ . + bmi")
fits[[4]] <- update(out0, ". ~ . + smoker")

## Generate predictions from each model:
preds <- lapply(fits, predict, newdat = ins2$valid)

## Estimate validation-set MSEs for each model:
mse <- sapply(preds, MSE, y_true = ins2$valid$charges)

## Find the smallest validation-set MSE:
mse
min(mse)
which.min(mse)
preds[[which.min(mse)]]

################################################################################
## PRACTICE PROBLEM 3.7
##
## Use the training data from PP 3.6 to complete the following tasks.
##
## a) Re-estimate the model from PP 3.3b.
## b) Re-estimate the model from PP 3.3c.
## c) Re-estimate the model from PP 3.3d.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.8
##
## Using the data from PP 3.6, complete the following tasks for each of the
## three models you estimated in PP 3.7a, PP 3.7b, and PP 3.7c.
##
## a) Compute validation-set predictions.
## b) Compute validation-set MSEs.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.9
##
## When comparing the models you estimated in PP 3.7a, PP 3.7b, and PP 3.7c,
## based on their relative prediction errors, which model should be preferred?
##
################################################################################

## Re-estimate the chosen model using the pooled training and validation data:
out4 <- update(fits[[which.min(mse)]], data = rbind(ins2$train, ins2$valid))
summary(out4)

## Estimate prediction error using the testing data:
MSE(y_pred = predict(out4, newdata = ins2$test), y_true = ins2$test$charges)

################################################################################
## PRACTICE PROBLEM 3.10
##
## a) Use the testing data that you set aside in PP 3.6 to estimate the
##    prediction error (i.e., test set MSE) of the model chosen in PP 3.9.
## b) Use the testing data that you set aside in PP 3.6 to estimate confidence
##    intervals for the predicted conditional means from the model chosen in
##    PP 3.9.
## c) Use the testing data that you set aside in PP 3.6 to estimate prediction
##    intervals for the model chosen in PP 3.9.
##
################################################################################


###-K-Fold Cross-Validation--------------------------------------------------###

## Cross-validate a single model using 10-fold cross-validation
cvOut <- 
  CVlm(data = ins, form.lm = charges ~ age + sex + children + region, m = 10)

## Check the results:
head(cvOut, 20)
attributes(cvOut)

## Extract the estimated CVE:
attr(cvOut, "ms")

## Use DAAG::CVlm() to cross-validate a list of models:
cvOut <- lapply(fits,
                function(f, data)
                    CVlm(data    = data,
                         form.lm = f,
                         m       = 10,
                         seed    = 235711,
                         plotit  = FALSE),
                data = train)

## Extract the cross-validation errors:
(cve <- sapply(cvOut, attr, which = "ms"))

## Find the smallest cross-validation error:
which.min(cve)

################################################################################
## PRACTICE PROBLEM 3.11
##
## Randomly split the sample into disjoint training and testing sets with sample
## sizes of N = 800 and N = 210, respectively.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.12
##
## Use the training data from PP 3.11 to run 5-fold cross-validation comparing
## the three models defined in PP 3.3b, PP 3.3c, and PP 3.3c.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 3.13
##
## a) When comparing the models you tested in PP 3.12 based on their relative
##    cross-validation errors, which model should be preferred?
## b) Do the cross-validation results agree with the results from PP 3.5b and
##    PP 3.9?
## c) Use the testing data that you set aside in PP 3.11 to estimate the
##    prediction error (i.e., test set MSE) of the model chosen in (a).
##
################################################################################


###-END----------------------------------------------------------------------###
