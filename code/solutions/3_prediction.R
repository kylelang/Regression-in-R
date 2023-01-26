### Title:    Suggested Solutions 3: Prediction
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2023-01-26

library(MLmetrics)
library(DAAG)
library(dplyr)

###-Data I/O-----------------------------------------------------------------###

## 3.1) Use the readRDS() function to load the "yps.rds" dataset.

dataDir <- "data/"
yps     <- readRDS(paste0(dataDir, "yps.rds"))


###-Prediction & Simple Split-Sample Cross-Validation------------------------###

## 3.2) Randomly split the sample into disjoint training and testing sets with
##      sample sizes of N = 800 and N = 210, respectively.

ind   <- sample(1:nrow(yps))
train <- yps[ind[1:800], ]
test  <- yps[ind[801:1010], ]

###--------------------------------------------------------------------------###

## 3.3a) Use the training data to estimate a baseline model that regresses
##       "Number.of.friends onto "Age" and "Gender".

out0 <- lm(Number.of.friends ~ Age + Gender, data = train)
summary(out0)

## 3.3b) Update the baseline model (from 2a) by adding "Keeping.promises",
##       "Empathy", "Friends.versus.money", and "Charity" as additional
##       predictors.

out1 <- list()

out1$b <-
    update(out0,
           ". ~ . + Keeping.promises + Empathy + Friends.versus.money + Charity"
           )
summary(out1$b)

## 3.3c) Update the baseline model (from 2a) by adding "Branded.clothing",
##       "Entertainment.spending", "Spending.on.looks", and "Spending.on.gadgets"
##       as additional predictors.

out1$c <-
    update(out0,
           ". ~ . + Branded.clothing + Entertainment.spending + Spending.on.looks + Spending.on.gadgets"
           )
summary(out1$c)

## 3.3d) Update the baseline model (from 2a) by adding "Workaholism",
##       "Reliability", "Responding.to.a.serious.letter", and "Assertiveness" as
##       additional predictors.

out1$d <-
    update(out0,
           ". ~ . + Workaholism + Reliability + Responding.to.a.serious.letter + Assertiveness"
           )
summary(out1$d)

###--------------------------------------------------------------------------###

## 3.4a) Compute training-set predictions.

trainPred1 <- lapply(out1, predict)

## 3.4b) Compute training-set MSEs.

trainMse1 <- sapply(trainPred1, MSE, y_true = train$Number.of.friends)
trainMse1

## 3.4c) Compute test-set predictions.

testPred1 <- lapply(out1, predict, newdata = test)

## 3.4d) Compute test-set MSEs.

testMse1 <- sapply(testPred1, MSE, y_true = test$Number.of.friends)
testMse1

###--------------------------------------------------------------------------###

## 3.5a) When comparing the models you estimated in PP 3.3b, PP 3.3c, and
##       PP 3.3d based on their relative training-set prediction errors, which
##       model should be preferred?

trainMse1 %>% which.min() %>% names() %>% paste0("PP 3.3", .)

## 3.5b) When comparing the models you estimated in PP 3.3b, PP 3.3c, and
##       PP 3.3d  based on their relative test-set prediction errors, which
##       model should be preferred?

testMse1 %>% which.min() %>% names() %>% paste0("PP 3.3", .)


###-Three-Way Split----------------------------------------------------------###

## 3.6) Randomly split the sample into disjoint training, validation, and
##      testing sets with sample sizes of N = 700, N = 155, and N = 155,
##      respectively.

ind   <- sample(c(rep("train", 700), rep("valid", 155), rep("test", 155)))
yps2  <- split(yps, ind)

###--------------------------------------------------------------------------###

## 3.7a) Re-estimate the model from PP 3.3b.
## 3.7b) Re-estimate the model from PP 3.3c.
## 3.7c) Re-estimate the model from PP 3.3d.

### Answer for 3.7a, 3.7b, 3.7c:
out2        <- lapply(out1, update, data = yps2$train)
names(out2) <- letters[1:3]

###--------------------------------------------------------------------------###

## 3.8a) Compute the validation-set predictions.

validPred2 <- lapply(out2, predict, newdata = yps2$valid)

## 3.8b) Compute the validation-set MSEs.

validMse2 <- sapply(validPred2, MSE, y_true = yps2$valid$Number.of.friends)
validMse2

###--------------------------------------------------------------------------###

## 3.9) When comparing the models you estimated in PP 3.7a, PP 3.7b, and
##      PP 3.7c, based on their relative prediction errors, which model should
##      be preferred?

validMse2 %>% which.min() %>% names() %>% paste0("PP 3.7", .)

###--------------------------------------------------------------------------###

## 3.10a) Use the testing data that you set aside in PP 3.6 to estimate the
##    prediction error (i.e., test set MSE) of the model chosen in PP 3.9.

out3 <- update(out2[[which.min(validMse2)]],
               data = rbind(yps2$train, yps2$valid)
               )
testMse3 <- MSE(y_pred = predict(out3, newdata = yps2$test),
                y_true = yps2$test$Number.of.friends)
testMse3

## 3.10b) Use the testing data that you set aside in PP 3.6 to estimate
##        confidence intervals for the predicted conditional means from the
##        model chosen in PP 3.9.

predict(out3, newdata = yps2$test, interval = "confidence")

## 3.10c) Use the testing data that you set aside in PP 3.6 to estimate
##        prediction intervals for the model chosen in PP 3.9.

predict(out3, newdata = yps2$test, interval = "prediction")


###-K-Fold Cross-Validation--------------------------------------------------###

## 3.11) Randomly split the sample into disjoint training and testing sets with
##       sample sizes of N = 800 and N = 210, respectively.

index <- c(rep("train", 800), rep("test", 210)) %>% sample()
yps2  <- split(yps, index)

###--------------------------------------------------------------------------###

## 3.12)  Use the training data from PP 3.11 to run 5-fold cross-validation
##        comparing the three models defined in PP 3.3b, PP 3.3c, and PP 3.3c.

### Perform the cross-validation:
cvOut <- lapply(out2,
                function(f, data)
                  CVlm(data    = data,
                       form.lm = f,
                       m       = 5,
                       seed    = 235711,
                       plotit  = FALSE),
                data = yps2$train)

(cve <- sapply(cvOut2, attr, which = "ms"))

###--------------------------------------------------------------------------###

## 3.13a) When comparing the models you tested in PP 3.12 based on their
##        relative cross-validation errors, which model should be preferred?

cve[which.min(cve)]

## 3.13b) Do the cross-validation results agree with the results from PP 3.5b
##        and PP 3.9?

c(which.min(testMse1), which.min(validMse2), which.min(cve)) %>%
  unique() %>%
  length() %>%
  {. == 1} %>% 
  ifelse("YES", "NO")

## 3.13c) Use the testing data that you set aside in PP 3.11 to estimate the
##        prediction error (i.e., test set MSE) of the model chosen in (a).

MSE(y_pred = predict(fits[[which.min(cve)]], newdata = yps2$test),
    y_true = yps2$test$Number.of.friends)


###-END----------------------------------------------------------------------###
