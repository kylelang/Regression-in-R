### Title:    Stats & Methods Lab 6 Suggested Solutions
### Author:   Kyle M. Lang
### Created:  2018-10-09
### Modified: 2020-10-09


###          ###
### Overview ###
###          ###

## You will practice regression diagnostics for MLR models.

## You will need the "airQual.rds" dataset which is available in the "data"
## directory for this lab.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) Use the "install.packages" function to install the "moments", "lmtest",
##    and "sandwich" packages.

install.packages(c("moments", "lmtest", "sandwich"),
                 repos = "http://cloud.r-project.org")

## 2) Use the "library" function to load the "moments", "lmtest", and "sandwich"
##    packages.

library(moments)
library(lmtest)
library(sandwich)

## 3) Use the "readRDS" function to load the "airQual.rds" dataset into your
##    workspace.

dataDir <- "../data/"
airQual <- readRDS(paste0(dataDir, "airQual.rds"))


##--Model specification-------------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1) Regress "Temp" onto "Ozone", "Wind", and "Solar.R".

out1 <- lm(Temp ~ Ozone + Wind + Solar.R, data = airQual)
summary(out1)

## 2a) Plot the residuals from the model estimated in (1) against its fitted
##     values.

plot(x = predict(out1), y = resid(out1))
abline(h = 0, col = "gray")

## 2b) Add a loess line to the residual plot from (2a).

scatter.smooth(x = predict(out1), y = resid(out1))

## 2b) What can you infer from the plots created in (2a) and (2b)?

### THE MODEL APPEARS TO BE GROSSLY MISSPECIFIED.

## 2c) What do you think is the best course of action to correct the issues
##     represented in the plot from (2a)?

### WE PROBABLY NEED TO INCLUDE SOME NONLINEAR TERMS.

## 3a) Conduct a Ramsey RESET for the model estimated in (1).
##     -- Add the second and third powers of the fitted values.

resettest(out1)

## 3b) What do the results of the RESET in (3a) tell you?

### THE MODEL IS MISSPECIFIED.

## 4a) Update the model estimated in (1) three times. In each new model, add the
##     square of exactly one of the predictor variables.
##     -- Each of these three models should be identical to the model from (1)
##        except for the inclusion of a different quadratic term.

vars <- c("Solar.R", "Ozone", "Wind")
fits <- lapply(vars,
               function(x, base) update(base, paste0(". ~ . + I(", x, "^2)")),
               base = out1)
names(fits) <- vars

lapply(fits, summary)

## 4b) For each of the updated models estimated in (4a) compute the same type of
##     residual plot that you created in (2b) and conduct a Ramsey RESET as you
##     did in (3a).

par(mfrow = c(1, 3))
for(v in vars)
    scatter.smooth(x = predict(fits[[v]]), y = resid(fits[[v]]), main = v)

reset <- lapply(fits, resettest)
reset

## 4c) Which predictor's quadratic term most improved the model specification?

best <- which.min(sapply(reset, "[[", x = "statistic"))
names(reset)[best]

## 4d) Does the RESET for the model you indicated in (4c) still suggest
##     significant misspecification?

check <- reset[[best]]$p.value < 0.05
ifelse(check, "YES", "NO")


##--Diagnostics---------------------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1) Regress "Temp" onto "Ozone", "Wind", "Solar.R", and the square of "Ozone".
##    -- In the following sections, this model will be referred to as "M0".

m0 <- lm(Temp ~ Ozone + Wind + Solar.R + I(Ozone^2), data = airQual)
summary(m0)

## 2a) Plot the residuals from the model estimated in (1) against its fitted
##     values, and add a loess line to the plot.

res0  <- resid(m0)
pred0 <- predict(m0)

scatter.smooth(x = pred0, y = res0)
abline(h = 0, col = "gray")

## 2b) What can you infer from the plot created in (2a))?

### THE MODEL DOES NOT APPEAR TO BE MISSPECIFIED, BUT THERE MAY BE SOME
### HETEROSCEDASTICITY.

## 4a) Conduct a Breusch-Pagan test for the model estimated in (1).

bptest(m0)

## 4b) What does the Breusch-Pagan test you conducted in (4a) tell you?

### THE HYPOTHESIS OF HOMOSCEDASTICITY IS REJECTED, SO THE ERRORS ARE
### NON-CONSTANT.

## 4c) Do the Breusch-Pagan test and the residual plot from (2a) agree?

### YES

## 5a) Evaluate the normality of the residuals from the model in (1) using a Q-Q
##     plot, the skewness, the kurtosis, the Shapiro-Wilk test, and the
##     Kolmogorov-Smirnov test.

qqnorm(res0)
qqline(res0)

skewness(res0)
kurtosis(res0)

shapiro.test(res0)
ks.test(x = res0, y = pnorm, mean = mean(res0), sd = sd(res0))

## 5b) Do the results of the diagnostics you conducted for (5a) agree?

### YES

## 5c) Create a kernel density plot of the residuals from the model in (1).

plot(density(res0))

## 5d) Judging by the information gained in (5a) and (5b), do you think it's
##     safe to assume normally distributed errors for the model in (1)?

### YEAH, PROBABLY SO.


##--Robust SEs----------------------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1) Estimate the heteroscedasticity consistent (HC) asymptotic covariance
##    matrix for M0 (i.e., the model from [1] in the "Diagnostics" section).

covHC0 <- vcovHC(m0)

## 2a) Use the HC covariance matrix from (1) to test the coefficients of M0 with
##     robust SEs.

coeftest(m0, vcov = covHC0)

## 2b) Compare the results from (2a) to the default tests of M0's coefficients.
##     What changes when using robust SEs?

summary(m0)$coefficients

### THE ROBUST STANDARD ERRORS ARE A LITTLE BIT LARGER THAN THEIR DEFAULT
### COUNTERPARTS.

## 3) Update M0 by adding the squares of "Wind" and "Solar.R".

m0.2 <- update(m0, ". ~ . + I(Wind^2) + I(Solar.R^2)")
summary(m0.2)

## 4a) Using HC estimates of the SEs, conduct a nested model comparison to test
##     if adding the squares of "Wind" and "Solar.R" to M0 explains
##     significantly more variance in "Temp".

waldtest(m0, m0.2, vcov = vcovHC)

## 4b) What is the conclusion of the test you conducted in (4a)?

### ADDING THE TWO NEW TERMS DOES NOT EXPLAIN A SIGNIFICANTLY GREATER
### PROPORTION OF VARIABILITY IN "TEMP".

## 4c) Compare the test in (4a) to the default version that does not use HC
##     estimates of the SEs. What differs when using robust SEs?

anova(m0, m0.2)
### THE ROBUST VERSION PRODUCES A LARGER TEST STATISTIC.


##--Influential observations--------------------------------------------------##

### Use the "airQual" data to complete the following:

## 1a) Compute the studentized residuals of M0 (i.e., the model from [1] in the
##     "Diagnostics" section).

sr0 <- rstudent(m0)

## 1b) Create an index plot of the residuals computed in (1a).

plot(sr0)

## 1c) What can you infer from the plot in (1b)?

### TWO OBSERVATIONS APPEAR TO BE POTENTIAL OUTLIERS (I.E., THEY HAVE
### ABSOLUTE STUDENTIZED RESIDUALS LARGER THAN 3).

## 1d) What are the observation numbers for the two most probable outliers
##     according to the residuals from (1a)?

### Define a function to identify the n biggest values in x:
### NOTE: This function will only work when the vector has "numeric" names.
findBiggest <- function(x, n)
    as.numeric(names(sort(x, decreasing = TRUE)[1 : n]))

badSr <- findBiggest(sr0, 2)
badSr

## 2a) Compute the leverages of M0.

lv0 <- hatvalues(m0)

## 2b) Create an index plot of the leverages computed in (2a).

plot(lv0)

## 2c) What can you infer from the plot in (2b)?

### THREE OBSERVATIONS SEEM TO HAVE ESPECIALLY HIGH LEVERAGES.

## 2d) What are the observation numbers with the three highest leverages?

badLv <- findBiggest(lv0, 3)
badLv

## 3a) Compute the Cook's distances of M0.

cd0 <- cooks.distance(m0)

## 3b) Create an index plot of the distances computed in (3a).

plot(cd0)

## 3c) What can you infer from the plot in (3b)?

### FIVE OBSERVATIONS APPEAR TO BE ESPECIALLY INFLUENTIAL.

## 3d) What are the observation numbers for the five most influential cases
##     according to the distances from (3a)?

badCd <- findBiggest(cd0, 5)
badCd

## 4a) Compute the DFFITS of M0.

df0 <- dffits(m0)

## 4b) Create an index plot of the DFFITS computed in (4a).

plot(df0)

## 4c) What can you infer from the plot in (4b)?

### FOUR OR FIVE CASES APPEAR TO BE POTENTIALLY INFLUENTIAL.

## 4d) What are the observation numbers for the five most influential cases
##     according to the DFFITS from (4a)?

badDf <- findBiggest(abs(df0), 5)
badDf

## 4e) Do the results from (4d) agree with the results from (3d)?

check <- all.equal(sort(badDf), sort(badCd))
ifelse(check, "YES", "NO")

## 4e) What do you notice about the set of observations flagged as influential
##     cases in (4d) relative to the observations flagged as high leverage
##     points in (2d) and those flagged as outliers in (1d)?

badSr
badLv
badDf

### ALL OF THE HIGH-LEVERAGE CASES WERE FLAGGED AS INFLUENTIAL BUT ONLY ONE OF
### THE TWO OUTLIERS WAS FLAGGED AS INFLUENTIAL. THERE WAS ALSO AN INFLUENTIAL
### CASE THAT WAS NOT AN OUTLIER OR A HIGH-LEVERAGE POINT.

## 5a) Remove the five most influential cases from (4d), and use the cleaned
##     data to rerun M0.

m0.3 <- update(m0, data = airQual[-infObs, ])
summary(m0.3)

## 5b) Compare the results of the model in (5a) to the results of the original
##     M0. What changes when removing the influential cases?

summary(m0)
### THE R-SQUARED FROM THE CLEANED MODEL IS LARGER AS ARE ALL OF THE SLOPES.

##----------------------------------------------------------------------------##
