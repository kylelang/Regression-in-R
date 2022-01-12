### Title:    Stats & Methods Lab 3 Suggested Solutions
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2020-09-23


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

library(MLmetrics)
library(Rcpp) # Loading Rcpp may be necessary to work around a bug wherein MICE fails to import some Rcpp functions.
library(mice)

## 2) Use the "source" function to source the "studentFunctions.R" script.

source("studentFunctions.R")

## 3) Use the "paste0" function and the "readRDS" function to load the "yps.rds"
##    and "bfiANC2.rds" datasets into your workspace.

dataDir <- "../data/"
plotDir <- "../plots/" # We'll use this for the MI plots below
yps     <- readRDS(paste0(dataDir, "yps.rds"))
bfi     <- readRDS(paste0(dataDir, "bfiANC2.rds"))

## 4) Use the "set.seed" function to set the random number seed.

set.seed(235711)


##--Prediction/Split-Sample Cross-Validation----------------------------------##

### Use the "yps" data to complete the following:

## 1) Randomly split the sample into disjoint training and testing sets with
##    sample sizes of N = 800 and N = 210, respectively.

ind   <- sample(1 : nrow(yps))
train <- yps[ind[1 : 800], ]
test  <- yps[ind[801 : 1010], ]

### OR ###

ind   <- sample(c(rep("train", 800), rep("test", 210)))
tmp   <- split(yps, ind)
train <- tmp$train
test  <- tmp$test

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

train <- yps[ind[1 : 700], ]
valid <- yps[ind[701 : 855], ]
test  <- yps[ind[856 : 1010], ]

### OR ###

ind   <- sample(c(rep("train", 700), rep("valid", 155), rep("test", 155)))
yps2  <- split(yps, ind)
train <- yps2$train
valid <- yps2$valid
test  <- yps2$test

## 5a) Use the training data from (4) to re-estimate the model from (2b).
## 5b) Use the training data from (4) to re-estimate the model from (2c).
## 5c) Use the training data from (4) to re-estimate the model from (2d).

### Answer for (5a), (5b), (5c):
out5        <- lapply(out2, update, data = train)
names(out5) <- letters[1 : 3]

## 6a) Compute the validation-set predictions from the three models you
##     estimated in (5a), (5b), and (5c).

validPred6 <- lapply(out5, predict, newdata = valid)

## 6b) Compute the validation-set MSEs for the three models you estimated in
##     (5a), (5b), and (5c).

validMse6 <- sapply(validPred6, MSE, y_true = valid$Number.of.friends)
validMse6

## 6c) When comparing the models you estimated in (5a), (5b), and (5c) based on
##     their relative prediction errors, which model should be preferred?

paste0(5, names(which.min(validMse6)))

## 7) Use the testing data that you set aside in (4) to estimate the prediction
##    error (i.e., test set MSE) of the model chosen in (6c).

out7 <- update(out5[[which.min(validMse6)]], data = rbind(train, valid))
mse7 <- MSE(y_pred = predict(out7, newdata = test),
            y_true = test$Number.of.friends)
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
##       "Keeping.promises", "Empathy", "Friends.versus.money", and "Charity".
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Branded.clothing", "Entertainment.spending", "Spending.on.looks", and
##       "Spending.on.gadgets".
##    -- A model regressing "Number.of.friends onto "Age" and "Gender",
##       "Workaholism", "Reliability", "Responding.to.a.serious.letter", and
##       "Assertiveness".

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
cve <- cv.lm(data = yps2$train, models = mods, K = 5, seed = 235711)

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

### Generate a method vector:
meth        <- rep("", ncol(bfi))
names(meth) <- colnames(bfi)

meth[grep("^A\\d", names(meth))] <- "pmm"
meth[grep("^N\\d", names(meth))] <- "norm"
meth[grep("^C\\d", names(meth))] <- "norm.boot"
meth["education"]                <- "polyreg"

### Check the method vector:
meth

### Generate a predictor matrix:
preds <- quickpred(data    = bfi,
                   mincor  = 0.25,
                   include = c("age", "gender"),
                   exclude = "id")

### Check the predictor matrix:
preds

### Impute the missing data:
miceOut <- mice(data            = bfi,
                m               = 25,
                maxit           = 15,
                predictorMatrix = preds,
                method          = meth,
                seed            = 314159)

## 2a) Create traceplots of the imputed values' means and SDs to check that the
##     imputation models converged.

pdf(paste0(plotDir, "miceTracePlots.pdf"), onefile = TRUE)
plot(miceOut)
dev.off()

## 2b) Create overlaid density plots of the imputed vs. observed values to
##     sanity-check the imputaitons.

pdf(paste0(plotDir, "miceDensityPlots.pdf"), onefile = TRUE)
densityplot(miceOut)
dev.off()

## 2c) Based on the plots you created in (2a) and (2b), would you say that the
##     imputations are valid?

### YES. THE IMPUTATION MODELS LOOK TO HAVE CONVERGED AND THE IMPUTED VALUES
### SEEM REASONABLE.

## 3a) Use the "lm.mids" function to regress "A1" onto "C1", "N1", and
##     "education" using the multiply imputed data from (1)

miFit1 <- lm.mids(A1 ~ C1 + N1 + education, data = miceOut)
summary(miFit1)

## 3b) Use the "lm.mids" function to regress "A1" onto "C1", "N1", "education",
##     "age", and "gender" using the multiply imputed data from (1)

miFit2 <- lm.mids(A1 ~ C1 + N1 + education + age + gender, data = miceOut)
summary(miFit2)

## 4a) What is the MI estimate of the slope of "age" on "A1" from (3b)?

miPool2 <- pool(miFit2)
tmp     <- miPool2$pooled[miPool2$pooled == "age", ]
tmp["estimate"]


## 4b) Is the effect in (4a) significant at the alpha = 0.05 level?

### Compute the t-statistic and p-value:
tStat <- with(tmp, estimate / sqrt(t))
pVal  <- 2 * pt(q = abs(tStat), df = tmp$df, lower.tail = FALSE)

ifelse(pVal < 0.05, "YES", "NO")

## 5a) What is the MI estimate of the slope of "N1" on "A1" from (3a)?

miPool1 <- pool(miFit1)
tmp     <- miPool1$pooled[miPool1$pooled == "N1", ]
tmp["estimate"]

## 5b) Is the effect in (5a) significant at the alpha = 0.01 level?

### Compute the t-statistic and p-value:
tStat <- with(tmp, estimate / sqrt(t))
pVal  <- 2 * pt(q = abs(tStat), df = tmp$df, lower.tail = FALSE)

ifelse(pVal < 0.01, "YES", "NO")

## 6a) What is the MI estimate of the R^2 from the model in (3a)?

r2.1 <- pool.r.squared(miFit1)[ , "est"]
r2.1

## 6b) What is the MI estimate of the R^2 from the model in (3b)?

r2.2 <- pool.r.squared(miFit2)[ , "est"]
r2.2

## 7a) What is the MI estimate of the increase in R^2 when going from the model
##     in (3a) to the model in (3b)?

r2.2 - r2.1

## 7b) Is the increase in R^2 from (7a) statistically significant at the
##     alpha = 0.05 level?

out7b <- D2(miFit2, miFit1)
out7b

check <- out7b$result[ , "P(>F)"] < 0.05
ifelse(check, "YES", "NO")

## 7c) What is the value of the test statistic that you used to answer (7b)?

out7b$result[ , "F.value"]
