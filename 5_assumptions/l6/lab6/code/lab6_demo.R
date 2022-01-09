### Title:    Stats & Methods Lab 6 Demonstration Script
### Author:   Kyle M. Lang
### Created:  2017-10-08
### Modified: 2020-10-09

install.packages(c("lmtest", "moments", "sandwich"),
                 repos = "http://cloud.r-project.org")

rm(list = ls(all = TRUE))

library(MASS)     # For the 'Cars93' data
library(lmtest)   # Provides Breusch-Pagan and RESET Tests
library(moments)  # Provides skewness and kurtosis
library(sandwich) # Provided HC variance estimators

## Load some data:
data(Cars93)

##----------------------------------------------------------------------------##

## Check skewness of 'Price'
skewness(Cars93$Price)

## Create a kernel density plot of 'Price'
plot(density(Cars93$Price))

## Check kurtosis of 'Horsepower'
kurtosis(Cars93$Horsepower)

## Is this excess kurtosis?
kurtosis(rnorm(10000))

##----------------------------------------------------------------------------##

out1 <- lm(Price ~ Horsepower + MPG.city + Passengers, data = Cars93)
summary(out1)

## Save the residuals and fitted values from out1:
res1  <- resid(out1)
yHat1 <- predict(out1)

## Make a simple residual plot:
plot(y = res1, x = yHat1)
abline(h = 0, col = "gray")

## Create a residual plot with a superimposed loess line:
scatter.smooth(y     = res1,
               x     = yHat1,
               span  = 0.5,
               lpars = list(col = "red")
               )
abline(h = 0, col = "gray")

## Check normality of residuals via skewness and kurtosis:
skewness(res1)
kurtosis(res1)

## Check the normality of residual via Q-Q Plot:
qqnorm(res1)
qqline(res1)

## Check the normality of residuals via Shapiro-Wilk test and KS test:
shapiro.test(res1)
ks.test(x = res1, y = pnorm, mean = mean(res1), sd = sd(res1))

## Residuals vs. Predicted plot:
plot(out1, which = 1)

## Q-Q Plot:
plot(out1, which = 2)

## Scale-Location Plot:
plot(out1, which = 3)

## Cook's Distance Plot:
plot(out1, which = 4)

## Residuals vs. Leverage Plot:
plot(out1, which = 5)

##----------------------------------------------------------------------------##

## Run the Breusch-Pagan Test:
bptest(out1)

## Compute HC estimate of the ACOV:
covHC1 <- vcovHC(out1)

## Do a robust test of the coefficients:
coeftest(out1, vcov = covHC1)

## Compare the default version:
summary(out1)$coefficients

## Extend 'out1' by adding some interactions:
out1.2 <- update(out1, ". ~ . + Horsepower*MPG.city + Passengers*MPG.city")
summary(out1.2)

## Compare the 'out1' and 'out1.2' models using robust SEs:
waldtest(out1, out1.2, vcov = vcovHC)

### OR ###

covHC1.2 <- vcovHC(out1.2)
waldtest(out1, out1.2, vcov = covHC1.2)

##----------------------------------------------------------------------------##

out2 <- lm(MPG.city ~ Horsepower + Fuel.tank.capacity + Weight, data = Cars93)
summary(out2)

## Residuals vs. Predicted plot:
plot(out2, which = 1)

## Q-Q Plot:
plot(out2, which = 2)

## Scale-Location Plot:
plot(out2, which = 3)

## Cook's Distance Plot:
plot(out2, which = 4)

## Residuals vs. Leverage Plot:
plot(out2, which = 5)

##----------------------------------------------------------------------------##

## Run the Ramsey RESET:
resettest(out2)

## Update the model:
out2.1 <- update(out2, ". ~ . + I(Horsepower^2)")
summary(out2.1)
resettest(out2.1)

## Residuals vs. Predicted plot:
plot(out2.1, which = 1)

## Q-Q Plot:
plot(out2.1, which = 2)

## Scale-Location Plot:
plot(out2.1, which = 3)

## Cook's Distance Plot:
plot(out2.1, which = 4)

## Residuals vs. Leverage Plot:
plot(out2.1, which = 5)

## Update the model:
out2.2 <- update(out2, ". ~ . + I(Weight^2)")
summary(out2.2)
resettest(out2.2)

## Residuals vs. Predicted plot:
plot(out2.2, which = 1)

## Q-Q Plot:
plot(out2.2, which = 2)

## Scale-Location Plot:
plot(out2.2, which = 3)

## Cook's Distance Plot:
plot(out2.2, which = 4)

## Residuals vs. Leverage Plot:
plot(out2.2, which = 5)

##----------------------------------------------------------------------------##

## Externally studentized residuals to check for outliers:
sr2 <- rstudent(out2.2)
sr2

## Create index plot of studentized residuals:
plot(sr2)

## Find outliers:
badSr2 <- which(abs(sr2) > 3)
badSr2

## Compute leverages to find high-leverage points:
lev2 <- hatvalues(out2.2)
lev2

## Create index plot of leverages:
plot(lev2)

## Find most extreme leverages:
lev2.s <- sort(lev2, decreasing = TRUE)
lev2.s

## Store the observation numbers for the most extreme leverages:
badLev2 <- as.numeric(names(lev2.s[1 : 3]))
badLev2

badLev2 <- which(lev2 > 0.2)
badLev2

##----------------------------------------------------------------------------##

## Compute a panel of leave-one-out influence measures:
im2 <- influence.measures(out2.2)
im2

## Compute measures of influence:
dff2 <- dffits(out2.2)
dff2

cd2  <- cooks.distance(out2.2)
cd2

dfb2 <- dfbetas(out2.2)
dfb2

## Create index plots for measures of influence:
plot(dff2)
plot(cd2)

plot(dfb2[ , 1])
plot(dfb2[ , 2])
plot(dfb2[ , 3])
plot(dfb2[ , 4])
plot(dfb2[ , 5])

## Find the single most influential observation:
maxCd   <- which.max(cd2)
maxDff  <- which.max(abs(dff2))
maxDfbI <- which.max(abs(dfb2[ , 1]))
maxDfbQ <- which.max(abs(dfb2[ , 5]))

maxCd
maxDff
maxDfbI
maxDfbQ

##----------------------------------------------------------------------------##

## Exclude the outliers:
Cars93.o <- Cars93[-badSr2, ]

## Refit the quadratic model:
out2.2o <- update(out2.2, data = Cars93.o)

summary(out2.2)
summary(out2.2o)

##----------------------------------------------------------------------------##

## Exclude the high-leverage points:
Cars93.l <- Cars93[-badLev2, ]

## Refit the quadratic model:
out2.2l <- update(out2.2, data = Cars93.l)

summary(out2.2)
summary(out2.2l)

##----------------------------------------------------------------------------##

## Exclude the most influential observation:
Cars93.i <- Cars93[-maxCd, ]

## Refit the quadratic model:
out2.2i <- update(out2.2, data = Cars93.i)

summary(out2.2)
summary(out2.2i)

##----------------------------------------------------------------------------##

## Exclude the observation with greatest influence on the quadratic term:
Cars93.q <- Cars93[-maxDfbQ, ]

## Refit the quadratic model:
out2.2q <- update(out2.2, data = Cars93.q)

summary(out2.2)
summary(out2.2q)

##----------------------------------------------------------------------------##


