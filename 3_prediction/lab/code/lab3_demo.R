### Title:    Stats & Methods Lab 3 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2020-09-16

###--------------------------------------------------------------------------###

### Preliminaries ###

rm(list = ls(all = TRUE)) # Clear workspace

## Source the "studentFunctions.R" script to get the cv.lm function:
source("studentFunctions.R")

## Source the "miPredictionRoutines.R" script to get MI-based prediction stuff:
source("miPredictionRoutines.R")

set.seed(235711) # Set the random number seed

library(MLmetrics) # We'll need this for MSEs
library(mice)      # We'll need this for MI
library(mitools)   # We'll need this for MI pooling

setwd("") # Let's all set our working directory to the correct place

###--------------------------------------------------------------------------###

### Data I/O ###

dataDir <- "../data/"
plotDir <- "../plots/"
ins     <- read.csv(paste0(dataDir, "insurance.csv")) 
bfi     <- readRDS(paste0(dataDir, "bfiOE.rds"))

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
mse[which.min(mse)]
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

###--------------------------------------------------------------------------###

### Multiple Imputation ###

colMeans(is.na(bfi))

### Imputation Step

## Conduct MI using all of the defaults:
miceOut1 <- mice(bfi)

## Set a seed and specify a different number of imputations and iterations:
miceOut2 <- mice(bfi, m = 20, maxit = 10, seed = 235711)

## Define our own method vector:
meth        <- rep("norm", ncol(bfi))
names(meth) <- colnames(bfi)

meth["gender"]    <- "logreg"
meth["education"] <- "polr"

## Impute missing using the method vector from above:
miceOut3 <- mice(bfi, m = 20, maxit = 10, method = meth, seed = 235711)

## Use mice::quickpred to generate a predictor matrix:
predMat <- quickpred(bfi, mincor = 0.2, include = "gender")
predMat

## Impute missing using the predictor matrix from above:
miceOut4 <-
    mice(bfi, m = 20, maxit = 10, predictorMatrix = predMat, seed = 235711)
ls(miceOut4)

## Create list of multiply imputed datasets:
impList <- complete(miceOut4, "all")

###--------------------------------------------------------------------------###

### Convergence Checks

## Create traceplots of imputed variables' means and SDs:
plot(miceOut4)
plot(miceOut4, layout = c(2, 5))
plot(miceOut4, "education")

## Write plots to external PDF file:
pdf(paste0(plotDir, "micePlots.pdf"), onefile = TRUE)
plot(miceOut4)
dev.off()

## Sanity check the imputations by plotting observed vs. imputed densities:
densityplot(miceOut4)
densityplot(miceOut4, ~O1)
densityplot(miceOut4, ~O1|.imp)

###--------------------------------------------------------------------------###

### Analysis Step

## Fit some regression models to the MI data:
fits1 <- lm.mids(E1 ~ gender, data = miceOut4)
fits2 <- lm.mids(E1 ~ gender + education, data = miceOut4)

## Fit a regression model to an arbitrary list of MI data:
fits3 <- lapply(impList,
                function(x) lm(E1 ~ age + gender + education, data = x)
                )

###--------------------------------------------------------------------------###

### Pooling Step

## Pool the fitted models:
poolFit1 <- pool(fits1)

## Summarize the pooled estimates:
summary(poolFit1)

## Compute the pooled R^2:
pool.r.squared(fits1)

## Compute increase in R^2:
pool.r.squared(fits2)[1] - pool.r.squared(fits1)[1]

## Do an F-test for the increase in R^2:
fTest <- pool.compare(fits2, fits1)

fTest$Dm     # Test statistic
fTest$pvalue # P-Value

## Pool an arbitrary list of fitted models:
poolFit3 <- MIcombine(fits3)

## Summarize pooled results:
summary(poolFit3)

## Compute wald tests from pooled results:
coef(poolFit3) / sqrt(diag(vcov(poolFit3)))

###--------------------------------------------------------------------------###

### MI-Based Prediction ###

### Prediction:
## Split the multiply imputed datasets into training and testing sets:
n <- nrow(impList[[1]])
index <- sample(
    c(rep("train", 400), rep("test", n - 400))
)

impList2 <- splitImps(imps = impList, index = index)

## Train a model on each multiply imputed training set:
fits <- lapply(impList2$train, function(x) lm(E1 ~ ., data = x))

## Generate imputation-specific predictions:
preds0 <- predictMi(fits = fits, newData = impList2$test, pooled = FALSE)
preds0

## Generate pooled predictions:
preds1 <- predictMi(fits = fits, newData = impList2$test, pooled = TRUE)
preds1

## Generate pooled predictions with confidence intervals:
predsCi <-
    predictMi(fits = fits, newData = impList2$test, interval = "confidence")
predsCi

## Generate pooled predictions with prediction intervals:
predsPi <-
    predictMi(fits = fits, newData = impList2$test, interval = "prediction")
predsPi

###--------------------------------------------------------------------------###

### MI-Based Prediction Cross-Validation ###

### Split-Sample Cross-Validation:

## Split the multiply imputed data into training, validation, and testing sets:
index2 <- sample(
    c(rep("train", 300), rep("valid", 130), rep("test", n - 430))
)

impList3 <- splitImps(imps = impList, index = index2)

## Define some models to compare:
mods <- c("E1 ~ gender + education + age",
          "E1 ~ E2 + E3 + E4 + E5",
          "E1 ~ E2 + E3 + E4 + E5 + O1 + O2 + O3 + O4 + O5",
          "E1 ~ .")

## Train the models and compute validation-set MSEs:
mse <- c()
for(m in mods) {
    fits     <- lapply(X   = impList3$train,
                       FUN = function(x, mod) lm(mod, data = x),
                       mod = m)
    mse[m] <- mseMi(fits = fits, newData = impList3$valid)
}

mse

## Merge the MI training a validations sets:
index3   <- gsub(pattern = "valid", replacement = "train", x = index2)
impList4 <- splitImps(impList, index3)

## Refit the winning model and compute test-set MSEs:
fits <- lapply(X   = impList4$train,
               FUN = function(x, mod) lm(mod, data = x),
               mod = mods[which.min(mse)])
mse <- mseMi(fits = fits, newData = impList4$test)

mse

### K-Fold Cross-Validation:

## Conduct 10-fold cross-validation in each multiply imputed dataset:
tmp <- sapply(impList4$train, cv.lm, K = 10, models = mods, seed = 235711)

## Aggregate the MI-based CVEs:
cve <- rowMeans(tmp)
cve

## Refit the winning model and compute test-set MSEs:
fits <- lapply(X   = impList4$train,
               FUN = function(x, mod) lm(mod, data = x),
               mod = mods[which.min(cve)])
mse <- mseMi(fits = fits, newData = impList4$test)

mse
