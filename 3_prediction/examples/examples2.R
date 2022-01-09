### Title:    Missing Data Course: Lecture 4 Simulation
### Author:   Kyle M. Lang
### Created:  2015-SEP-14
### Modified: 2015-SEP-14

            #install.packages("geoR", repos = "http://rweb.quant.ku.edu/cran")

rm(list = ls(all = TRUE))
                            
set.seed(235711)

library(mvtnorm)
library(mice)
library(pscl)
library(geoR)
library(mitools)

nObs <- 1000
pm <- 0.3
sigma <- matrix(c(1.0, 0.5, 0.0,
                  0.5, 1.0, 0.3,
                  0.0, 0.3, 1.0),
                ncol = 3)

simData <- as.data.frame(rmvnorm(nObs, c(0, 0, 0), sigma))
colnames(simData) <- c("y", "x", "z")
  
## Simulate MAR data:
missData <- simData
rVec <- pnorm(missData$x,
              mean = mean(missData$x),
              sd = sd(missData$x)) < pm
missData[rVec, "y"] <- NA

nMissing <- sum(rVec)
nObserved <- sum(!rVec)
yMiss <- missData[rVec, ]
yObs <- missData[!rVec, ]
missPredData <- as.matrix(cbind(1, yMiss[ , -1]))
obsPredData <- as.matrix(cbind(1, yObs[ , -1]))

## Get the imputation model moments:
lsFit <- lm(y ~ x + z, data = missData, na.action = "na.omit")
betaHat <- matrix(coef(lsFit))
sigma2Hat <- var(resid(lsFit))

## Get deterministic imputations:
imp1 <- missPredData %*% betaHat

## Fill missing cells in Y:
impData1 <- missData
impData1[rVec, "y"] <- imp1
                  
fit1 <- lm(y ~ z, impData1)
tVec1[rp] <- sqrt(summary(fit1)$fstat[1])
corVec1[rp] <- with(impData1, cor(x, y))

## Get stochastic imputations:
imp2 <-
  missPredData %*% betaHat +
  rnorm(nMissing, 0, sqrt(sigma2Hat))

## Fill missing cells in Y:
impData2 <- missData
impData2[rVec, "y"] <- imp2

fit2 <- lm(y ~ z, impData2)
tVec2[rp] <- sqrt(summary(fit2)$fstat[1])
corVec2[rp] <- with(impData2, cor(x, y))


## Do MI by hand:
nImps <- 100
nSams <- 5000

df <- nObserved - length(betaHat)
sigmaScale <- (1 / df) * crossprod(yObs$y - obsPredData %*% betaHat)

sigmaSams <- rinvchisq(nSams, df = df, scale = sigmaScale)

betaVarDenom <- solve(crossprod(obsPredData))

betaSams <- matrix(NA, nSams, length(betaHat))
for(m in 1 : nSams) {
  betaSigma <- sigmaSams[m] * betaVarDenom
  betaSams[m, ] <- rmvnorm(1, mean = betaHat, sigma = betaSigma)
}

impMat <- matrix(NA, nMissing, nSams)
for(m in 1 : nSams) {
  impMat[ , m] <-
    missPredData %*% matrix(betaSams[m, ]) +
          rnorm(nMissing, 0, sqrt(sigmaSams[m]))
}

par(mfrow = c(2, 3))
plot(density(betaSams[ , 1]),
     main = "Posterior Density of Intercept",
     xlab = "Intercept Value")
plot(density(betaSams[ , 2]),
     main = "Posterior Density of X Slope",
     xlab = "Slope Value")
plot(density(betaSams[ , 3]),
     main = "Posterior Density of Z Slope",
     xlab = "Slope Value")

for(i in 1 : 3) {
  obsNum <- c(1 : length(rVec))[rVec][i]
  plot(density(impMat[i, ]),
       main = paste0("Posterior Predictive Density of\nRow ",
         obsNum,
         "'s Missing Y Data"),
       xlab = "Y Value")
}

impList <- list()
impData3 <- missData
for(m in 1 : nImps) {
  impData3[rVec, "y"] <- impMat[ , m]
  impList[[m]] <- impData3
}

miCorList <-lapply(impList,
                   FUN = function(impDat){
                     with(impDat, cor(x, y))
                   }
                   )


