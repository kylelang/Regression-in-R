### Title:    Stats & Methods Lab 3 Demonstration Script 2
### Author:   Kyle M. Lang
### Created:  2017-09-08
### Modified: 2020-09-17

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
bfi     <- readRDS(paste0(dataDir, "bfiOE.rds"))

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
D1(fits2, fits1)

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

ls(impList2)
length(impList2$test)

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

ls(impList3)

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

ls(impList4)

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
