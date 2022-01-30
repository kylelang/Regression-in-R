### Title:    Regression in R: Lab 3 Suggested Solutions
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2022-01-30


###-Overview-----------------------------------------------------------------###

## You will practice prediction and cross-validation.

## You will need the "yps.rds" dataset to answer the following questions. This
## dataset is available in the "data" directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) Use the library() function to load the "MLmetrics" and "DAAG" packages.

library(MLmetrics)
library(DAAG)

## 2) Use the readRDS() function to load the "yps.rds" dataset.

dataDir <- "../../data/"
yps     <- readRDS(paste0(dataDir, "yps.rds"))

## 3) Use the set.seed() function to set the random number seed.

set.seed(235711)


###-Prediction/Split-Sample Cross-Validation---------------------------------###

### Use the "yps" data to complete the following:

## 1) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

ind   <- sample(1 : nrow(yps))
train <- yps[ind[1 : 800], ]
test  <- yps[ind[801 : 1010], ]

## 2a) Use the training data to estimate a baseline model that regresses
##     "Number.of.friends onto "Age" and "Gender".

out2a <- lm(Number.of.friends ~ Age + Gender, data = train)
summary(out2a)

## 2b) Update the baseline model (from 2a) by adding "Keeping.promises",
##     "Empathy", "Friends.versus.money", and "Charity" as additional
##     predictors.

out2 <- list()

out2[["b"]] <- update(out2a,
                      ". ~ . + Keeping.promises + Empathy + Friends.versus.money + Charity"
                      )
summary(out2[["b"]])

## 2c) Update the baseline model (from 2a) by adding "Branded.clothing",
##     "Entertainment.spending", "Spending.on.looks", and "Spending.on.gadgets"
##     as additional predictors.

out2[["c"]] <- update(out2a,
                      ". ~ . + Branded.clothing + Entertainment.spending + Spending.on.looks + Spending.on.gadgets"
                      )
summary(out2[["c"]])

## 2d) Update the baseline model (from 2a) by adding "Workaholism",
##     "Reliability", "Responding.to.a.serious.letter", and "Assertiveness" as
##     additional predictors.

out2[["d"]] <- update(out2a,
                      ". ~ . + Workaholism + Reliability + Responding.to.a.serious.letter + Assertiveness"
                      )
summary(out2[["d"]])

## 3a) Compute training-set predictions from the three models you estimated in
##     (2b), (2c), and (2d).

trainPred3 <- lapply(out2, predict)

## 3b) Compute training-set MSEs for the three models you estimated in (2b),
##     (2c), and (2d).

trainMse3 <- sapply(trainPred3, MSE, y_true = train$Number.of.friends)
trainMse3

## 3c) Compute test-set predictions from the three models you estimated in (2b),
##     (2c), and (2d).

testPred3 <- lapply(out2, predict, newdata = test)

## 3d) Compute test-set MSEs for the three models you estimated in (2b), (2c),
##     and (2d).

testMse3 <- sapply(testPred3, MSE, y_true = test$Number.of.friends)
testMse3

## 3e) When comparing the models you estimated in (2b), (2c), and (2d) based on
##     their relative training-set prediction errors, which model should be
##     preferred?

paste0(2, names(which.min(trainMse3)))

## 3f) When comparing the models you estimated in (2b), (2c), and (2d) based on
##     their relative test-set prediction errors, which model should be
##     preferred?

paste0(2, names(which.min(testMse3)))

## 4) Randomly split the sample into disjoint training, validation, and testing
##    sets with sample sizes of N = 700, N = 155, and N = 155, respectively.

ind   <- sample(c(rep("train", 700), rep("valid", 155), rep("test", 155)))
yps2  <- split(yps, ind)

## 5a) Use the training data from (4) to re-estimate the model from (2b).
## 5b) Use the training data from (4) to re-estimate the model from (2c).
## 5c) Use the training data from (4) to re-estimate the model from (2d).

### Answer for (5a), (5b), (5c):
out5        <- lapply(out2, update, data = yps2$train)
names(out5) <- letters[1:3]

## 6a) Compute the validation-set predictions from the three models you
##     estimated in (5a), (5b), and (5c).

validPred6 <- lapply(out5, predict, newdata = yps2$valid)

## 6b) Compute the validation-set MSEs for the three models you estimated in
##     (5a), (5b), and (5c).

validMse6 <- sapply(validPred6, MSE, y_true = yps2$valid$Number.of.friends)
validMse6

## 6c) When comparing the models you estimated in (5a), (5b), and (5c) based on
##     their relative prediction errors, which model should be preferred?

paste0(5, names(which.min(validMse6)))

## 7) Use the testing data that you set aside in (4) to estimate the prediction
##    error (i.e., test set MSE) of the model chosen in (6c).

out7 <- update(out5[[which.min(validMse6)]],
               data = rbind(yps2$train, yps2$valid)
               )
mse7 <- MSE(y_pred = predict(out7, newdata = yps2$test),
            y_true = yps2$test$Number.of.friends)
mse7


##--Prediction/K-Fold Cross-Validation----------------------------------------##

### Use the "yps" data to complete the following:

## 1) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

index <- sample(c(rep("train", 800), rep("test", 210)))
yps2  <- split(yps, index)

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

stem <- "Number.of.friends ~ Age + Gender"
mods <- paste(stem,
              c("Keeping.promises + Empathy + Friends.versus.money + Charity",
                "Branded.clothing + Entertainment.spending + Spending.on.looks + Spending.on.gadgets",
                "Workaholism + Reliability + Responding.to.a.serious.letter + Assertiveness"),
              sep = " + ")

### Check the formulas by training the models:
fits <- lapply(mods, lm, data = yps2$train)
lapply(fits, summary)

### Perform the cross-validation:
cvOut <- lapply(fits,
                function(f, data)
                    CVlm(data    = data,
                         form.lm = f,
                         m       = 5,
                         seed    = 235711,
                         plotit  = FALSE),
                data = yps2$train)

(cve <- sapply(cvOut, attr, which = "ms"))

## 3a) When comparing the models you tested in (2) based on their relative
##     cross-validation errors, which model should be preferred?

cve[which.min(cve)]

## 3b) Does the result from (3a) agree with the results from (3f) and (6c) in
##     the "Prediction/Split-Sample Cross-Validation" section?

tmp <- unique(c(which.min(cve), which.min(validMse6), which.min(testMse3)))
ifelse(length(tmp) == 1, "YES", "NO")

## 3c) Use the testing data that you set aside in (1) to estimate the prediction
##     error (i.e., test set MSE) of the model chosen in (3a).

MSE(y_pred = predict(fits[[which.min(cve)]], newdata = yps2$test),
    y_true = yps2$test$Number.of.friends)


###-END----------------------------------------------------------------------###
