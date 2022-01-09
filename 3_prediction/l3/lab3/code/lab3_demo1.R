### Title:    Stats & Methods Lab 3 Demonstration Script 1
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2020-09-17

###--------------------------------------------------------------------------###

### Preliminaries ###

rm(list = ls(all = TRUE)) # Clear workspace

## Source the "studentFunctions.R" script to get the cv.lm function:
source("studentFunctions.R")

set.seed(235711) # Set the random number seed

library(MLmetrics) # We'll need this for MSEs

setwd("") # Let's all set our working directory to the correct place

###--------------------------------------------------------------------------###

### Data I/O ###

dataDir <- "../data/"
ins     <- read.csv(paste0(dataDir, "insurance.csv")) 

###--------------------------------------------------------------------------###

### Prediction & Simple Split-Sample Cross-Validation ###

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

###--------------------------------------------------------------------------###

### Three-Way Split ###

## Split the sample into training, validation, and testing sets:
ind <- sample(
    c(rep("train", 800),
      rep("valid", 200),
      rep("test", nrow(ins) - 1000)
      )
)

table(ind)

ins2 <- split(ins, ind)
class(ins2)
ls(ins2)

## Fit four competing models:
out0 <- lm(charges ~ age + sex, data = ins2$train)
out1 <- update(out0, ". ~ . + children")
out2 <- update(out0, ". ~ . + region")
out3 <- update(out0, ". ~ . + bmi")
out4 <- update(out0, ". ~ . + smoker")

## Estimate validation-set MSEs for each model:
mse1 <- MSE(y_pred = predict(out1, newdata = ins2$valid),
            y_true = ins2$valid$charges)
mse2 <- MSE(y_pred = predict(out2, newdata = ins2$valid),
            y_true = ins2$valid$charges)
mse3 <- MSE(y_pred = predict(out3, newdata = ins2$valid),
            y_true = ins2$valid$charges)
mse4 <- MSE(y_pred = predict(out4, newdata = ins2$valid),
            y_true = ins2$valid$charges)

mseVec <- c(mse1, mse2, mse3, mse4)
mseVec

### OR ###

## Define a vector of the new X variables to add:
vars <- c("children", "region", "bmi", "smoker")

## Estimate the root model:
out0 <- lm(charges ~ age + sex, data = ins2$train)

## Estimate the four competing models using lapply:
out <- lapply(X   = vars,
              FUN = function(x, y) update(y, paste0(". ~ . + ", x)),
              y   = out0)

## Generate predictions from all four models using lapply:
preds <- lapply(out, predict, newdata = ins2$valid)

## Create a vector of MSEs using sapply:
mse <- sapply(preds, MSE, y_true = ins2$valid$charges)
mse

## Compare:
mse - mseVec

## Find the smallest validation-set MSE:
mse
min(mse)
which.min(mse)
preds[[which.min(mse)]]

## Re-estimate the chosen model using the pooled training and validation data:
out4.2 <- update(out[[which.min(mse)]], data = rbind(ins2$train, ins2$valid))
summary(out4.2)

## Estimate prediction error using the testing data:
MSE(y_pred = predict(out4.2, newdata = ins2$test), y_true = ins2$test$charges)

###--------------------------------------------------------------------------###

### K-Fold Cross-Validation ###

### Manually implement K-fold cross-validation

## Define some useful constants:
K <- 10
N <- nrow(ins)

## Create a vector of candidate models:
models <- c("charges ~ age + sex + children",
            "charges ~ age + sex + region",
            "charges ~ age + sex + bmi",
            "charges ~ age + sex + smoker")

## Create a partition vector:
part <- sample(rep(1 : K, ceiling(N / K)))[1 : N]

## Set indices for demo purposes:
m <- 1
k <- 1

## Loop over candidate models:
cve <- c()
for(m in 1 : length(models)) {
    ## Loop over K repititions:
    mse <- c()
    for(k in 1 : K) {
        ## Partition data:
        dat0 <- ins[part != k, ]
        dat1 <- ins[part == k, ]
        
        ## Fit model and generate predictions:
        fit  <- lm(models[m], data = dat0)
        pred <- predict(fit, newdata = dat1)
        
        ## Save MSE:
        mse[k] <- MSE(y_pred = pred, y_true = dat1$charges)
    }
    ## Save the CVE:
    cve[m] <- mean(mse) 
}

## Examine cross-validation errors:
cve
which.min(cve)

### Use the "cv.lm" function to do K-fold cross-validation

## Compare the four models from above using 10-fold cross-validation:
cve2 <- cv.lm(data   = ins,
              models = c("charges ~ age + sex + children",
                         "charges ~ age + sex + region",
                         "charges ~ age + sex + bmi",
                         "charges ~ age + sex + smoker"),
              K      = 10,
              seed   = 235711)

cve2
cve2[which.min(cve2)]
