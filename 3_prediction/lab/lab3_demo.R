### Title:    Regression in R: Lab 3 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2022-01-14


###-Preliminaries------------------------------------------------------------###

rm(list = ls(all = TRUE)) # Clear workspace

set.seed(235711) # Set the random number seed

library(MLmetrics) # For MSEs
library(dplyr)     # For pipes
library(DAAG)      # For cross-validation

setwd("") # Let's all set our working directory to the correct place


###-Data I/O-----------------------------------------------------------------###

dataDir <- "../../data/"
ins     <- read.csv(paste0(dataDir, "insurance.csv")) 


###-Prediction & Simple Split-Sample Cross-Validation------------------------###

## Split the data into training and testing sets:
ind   <- sample(1 : nrow(ins))
train <- ins[ind[1 : 1000], ]
test  <- ins[ind[1001 : nrow(ins)], ]

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

## Re-estimate the chosen model using the pooled training and validation data:
out4 <- update(fits[[which.min(mse)]], data = rbind(ins2$train, ins2$valid))
summary(out4)

## Estimate prediction error using the testing data:
MSE(y_pred = predict(out4, newdata = ins2$test), y_true = ins2$test$charges)


###-K-Fold Cross-Validation--------------------------------------------------###

## Cross-validate a single model using 10-fold cross-validation
CVlm(data = ins, form.lm = charges ~ age + sex + children + region, m = 10)

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


###-END----------------------------------------------------------------------###
